/* Handle parameterized types (templates) for GNU C++.
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Written by Ken Raeburn (raeburn@cygnus.com) while at Watchmaker Computing.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Known bugs or deficiencies include:
   * templates for class static data don't work (methods only)
   * duplicated method templates can crash the compiler
   * interface/impl data is taken from file defining the template
   * all methods must be provided in header files; can't use a source
     file that contains only the method templates and "just win"
   * method templates must be seen before the expansion of the
     class template is done
 */

#include "config.h"
#include <stdio.h>
#include "obstack.h"

#include "tree.h"
#include "flags.h"
#include "cp-tree.h"
#include "decl.h"
#include "parse.h"
#include "lex.h"
#include "output.h"
#include "defaults.h"

extern struct obstack permanent_obstack;

extern int lineno;
extern char *input_filename;
struct pending_inline *pending_template_expansions;

int processing_template_decl;
int processing_template_defn;

/* This is a kludge to handle instantiation of template methods that are
   used before their definition.  It should not be necessary after the
   template rewrite.  */
static tree template_classes;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

static int unify ();
static void add_pending_template ();

void overload_template_name (), pop_template_decls ();

/* We've got a template header coming up; set obstacks up to save the
   nodes created permanently.  (There might be cases with nested templates
   where we don't have to do this, but they aren't implemented, and it
   probably wouldn't be worth the effort.)  */
void
begin_template_parm_list ()
{
  pushlevel (0);
  push_obstacks (&permanent_obstack, &permanent_obstack);
  pushlevel (0);
}

/* Process information from new template parameter NEXT and append it to the
   LIST being built.  The rules for use of a template parameter type name
   by later parameters are not well-defined for us just yet.  However, the
   only way to avoid having to parse expressions of unknown complexity (and
   with tokens of unknown types) is to disallow it completely.	So for now,
   that is what is assumed.  */
tree
process_template_parm (list, next)
     tree list, next;
{
  tree parm;
  tree decl = 0;
  tree defval;
  int is_type;
  parm = next;
  my_friendly_assert (TREE_CODE (parm) == TREE_LIST, 259);
  defval = TREE_PURPOSE (parm);
  parm = TREE_VALUE (parm);
  is_type = TREE_PURPOSE (parm) == class_type_node;
  if (!is_type)
    {
      tree tinfo = 0;
      my_friendly_assert (TREE_CODE (TREE_PURPOSE (parm)) == TREE_LIST, 260);
      /* is a const-param */
      parm = grokdeclarator (TREE_VALUE (parm), TREE_PURPOSE (parm),
			     PARM, 0, NULL_TREE, NULL_TREE);
      /* A template parameter is not modifiable.  */
      TREE_READONLY (parm) = 1;
      if (IS_AGGR_TYPE (TREE_TYPE (parm)))
	{
	  sorry ("aggregate template parameter types");
	  TREE_TYPE (parm) = void_type_node;
	}
      tinfo = make_node (TEMPLATE_CONST_PARM);
      my_friendly_assert (TREE_PERMANENT (tinfo), 260.5);
      if (TREE_PERMANENT (parm) == 0)
        {
	  parm = copy_node (parm);
	  TREE_PERMANENT (parm) = 1;
        }
      TREE_TYPE (tinfo) = TREE_TYPE (parm);
      decl = build_decl (CONST_DECL, DECL_NAME (parm), TREE_TYPE (parm));
      DECL_INITIAL (decl) = tinfo;
      DECL_INITIAL (parm) = tinfo;
    }
  else
    {
      tree t = make_node (TEMPLATE_TYPE_PARM);
      decl = build_decl (TYPE_DECL, TREE_VALUE (parm), t);
      TYPE_MAIN_DECL (t) = decl;
      parm = decl;
      if (defval)
	{
	  if (IDENTIFIER_HAS_TYPE_VALUE (defval))
	    defval = IDENTIFIER_TYPE_VALUE (defval);
	  else
	    defval = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (defval));
	}
    }
  SET_DECL_ARTIFICIAL (decl);
  pushdecl (decl);
  parm = build_tree_list (defval, parm);
  return chainon (list, parm);
}

/* The end of a template parameter list has been reached.  Process the
   tree list into a parameter vector, converting each parameter into a more
   useful form.	 Type parameters are saved as IDENTIFIER_NODEs, and others
   as PARM_DECLs.  */

tree
end_template_parm_list (parms)
     tree parms;
{
  int nparms = 0;
  int saw_default = 0;
  tree saved_parmlist;
  tree parm;
  for (parm = parms; parm; parm = TREE_CHAIN (parm))
    nparms++;
  saved_parmlist = make_tree_vec (nparms);

  for (parm = parms, nparms = 0; parm; parm = TREE_CHAIN (parm), nparms++)
    {
      tree p = TREE_VALUE (parm);
      if (TREE_PURPOSE (parm))
	saw_default = 1;
      else if (saw_default)
	{
	  error ("if a default argument is given for one template parameter");
	  error ("default arguments must be given for all subsequent");
	  error ("parameters as well");
	}

      if (TREE_CODE (p) == TYPE_DECL)
	{
	  tree t = TREE_TYPE (p);
	  TEMPLATE_TYPE_SET_INFO (t, saved_parmlist, nparms);
	}
      else
	{
	  tree tinfo = DECL_INITIAL (p);
	  DECL_INITIAL (p) = NULL_TREE;
	  TEMPLATE_CONST_SET_INFO (tinfo, saved_parmlist, nparms);
	}
      TREE_VEC_ELT (saved_parmlist, nparms) = parm;
    }
  set_current_level_tags_transparency (1);
  processing_template_decl++;
  return saved_parmlist;
}

/* end_template_decl is called after a template declaration is seen.
   D1 is template header; D2 is class_head_sans_basetype or a
   TEMPLATE_DECL with its DECL_RESULT field set.  */
void
end_template_decl (d1, d2, is_class, defn)
     tree d1, d2, is_class;
     int defn;
{
  tree decl;
  struct template_info *tmpl;

  tmpl = (struct template_info *) obstack_alloc (&permanent_obstack,
					    sizeof (struct template_info));
  tmpl->text = 0;
  tmpl->length = 0;
  tmpl->aggr = is_class;

  /* cloned from reinit_parse_for_template */
  tmpl->filename = input_filename;
  tmpl->lineno = lineno;
  tmpl->parm_vec = d1;          /* [eichin:19911015.2306EST] */

  if (d2 == NULL_TREE || d2 == error_mark_node)
    {
      decl = 0;
      goto lose;
    }

  if (is_class)
    {
      decl = build_lang_decl (TEMPLATE_DECL, d2, NULL_TREE);
      GNU_xref_decl (current_function_decl, decl);
    }
  else
    {
      if (TREE_CODE (d2) == TEMPLATE_DECL)
	decl = d2;
      else
	{
	  /* Class destructor templates and operator templates are
	     slipping past as non-template nodes.  Process them here, since
	     I haven't figured out where to catch them earlier.  I could
	     go do that, but it's a choice between getting that done and
	     staying only N months behind schedule.  Sorry....  */
	  enum tree_code code;
	  my_friendly_assert (TREE_CODE (d2) == CALL_EXPR, 263);
	  code = TREE_CODE (TREE_OPERAND (d2, 0));
	  my_friendly_assert (code == BIT_NOT_EXPR
		  || code == OP_IDENTIFIER
		  || code == SCOPE_REF, 264);
	  d2 = grokdeclarator (d2, NULL_TREE, MEMFUNCDEF, 0, NULL_TREE, NULL_TREE);
	  decl = build_lang_decl (TEMPLATE_DECL, DECL_NAME (d2),
				  TREE_TYPE (d2));
	  DECL_TEMPLATE_RESULT (decl) = d2;
	  DECL_CONTEXT (decl) = DECL_CONTEXT (d2);
	  DECL_CLASS_CONTEXT (decl) = DECL_CLASS_CONTEXT (d2);
	  DECL_NAME (decl) = DECL_NAME (d2);
	  TREE_TYPE (decl) = TREE_TYPE (d2);
	  if (interface_unknown && flag_external_templates
	      && ! flag_alt_external_templates
	      && ! DECL_IN_SYSTEM_HEADER (decl))
	    warn_if_unknown_interface (decl);
	  TREE_PUBLIC (decl) = TREE_PUBLIC (d2) = flag_external_templates && !interface_unknown;
	  DECL_EXTERNAL (decl) = (DECL_EXTERNAL (d2)
				  && !(DECL_CLASS_CONTEXT (d2)
				       && !DECL_THIS_EXTERN (d2)));
	}

      /* All routines creating TEMPLATE_DECL nodes should now be using
	 build_lang_decl, which will have set this up already.	*/
      my_friendly_assert (DECL_LANG_SPECIFIC (decl) != 0, 265);

      /* @@ Somewhere, permanent allocation isn't being used.  */
      if (! DECL_TEMPLATE_IS_CLASS (decl)
	  && TREE_CODE (DECL_TEMPLATE_RESULT (decl)) == FUNCTION_DECL)
	{
	  tree result = DECL_TEMPLATE_RESULT (decl);
	  /* Will do nothing if allocation was already permanent.  */
	  DECL_ARGUMENTS (result) = copy_to_permanent (DECL_ARGUMENTS (result));
	}

      /* If this is for a method, there's an extra binding level here.	*/
      if (DECL_CONTEXT (DECL_TEMPLATE_RESULT (decl)) != NULL_TREE)
	{
	  /* @@ Find out where this should be getting set!  */
	  tree r = DECL_TEMPLATE_RESULT (decl);
	  if (DECL_LANG_SPECIFIC (r) && DECL_CLASS_CONTEXT (r) == NULL_TREE)
	    DECL_CLASS_CONTEXT (r) = DECL_CONTEXT (r);
	}
    }
  DECL_TEMPLATE_INFO (decl) = tmpl;
  DECL_TEMPLATE_PARMS (decl) = d1;

  /* So that duplicate_decls can do the right thing.  */
  if (defn)
    DECL_INITIAL (decl) = error_mark_node;
  
  /* If context of decl is non-null (i.e., method template), add it
     to the appropriate class template, and pop the binding levels.  */
  if (! is_class && DECL_CONTEXT (DECL_TEMPLATE_RESULT (decl)) != NULL_TREE)
    {
      tree ctx = DECL_CONTEXT (DECL_TEMPLATE_RESULT (decl));
      tree tmpl, t;
      my_friendly_assert (TREE_CODE (ctx) == UNINSTANTIATED_P_TYPE, 266);
      tmpl = UPT_TEMPLATE (ctx);
      for (t = DECL_TEMPLATE_MEMBERS (tmpl); t; t = TREE_CHAIN (t))
	if (TREE_PURPOSE (t) == DECL_NAME (decl)
	    && duplicate_decls (decl, TREE_VALUE (t)))
	  goto already_there;
      DECL_TEMPLATE_MEMBERS (tmpl) =
	perm_tree_cons (DECL_NAME (decl), decl, DECL_TEMPLATE_MEMBERS (tmpl));
    already_there:
      poplevel (0, 0, 0);
      poplevel (0, 0, 0);
    }
  /* Otherwise, go back to top level first, and push the template decl
     again there.  */
  else
    {
      poplevel (0, 0, 0);
      poplevel (0, 0, 0);
      pushdecl (decl);
    }
 lose:
#if 0 /* It happens sometimes, with syntactic or semantic errors.

	 One specific case:
	 template <class A, int X, int Y> class Foo { ... };
	 template <class A, int X, int y> Foo<X,Y>::method (Foo& x) { ... }
	 Note the missing "A" in the class containing "method".  */
  my_friendly_assert (global_bindings_p (), 267);
#else
  while (! global_bindings_p ())
    poplevel (0, 0, 0);
#endif
  pop_obstacks ();
  processing_template_decl--;
  (void) get_pending_sizes ();
}

tree tsubst		PROTO ((tree, tree*, int, tree));

/* Convert all template arguments to their appropriate types, and return
   a vector containing the resulting values.  If any error occurs, return
   error_mark_node.  */
static tree
coerce_template_parms (parms, arglist, in_decl)
     tree parms, arglist;
     tree in_decl;
{
  int nparms, nargs, i, lost = 0;
  tree vec;

  if (arglist == NULL_TREE)
    nargs = 0;
  else if (TREE_CODE (arglist) == TREE_VEC)
    nargs = TREE_VEC_LENGTH (arglist);
  else
    nargs = list_length (arglist);

  nparms = TREE_VEC_LENGTH (parms);

  if (nargs > nparms
      || (nargs < nparms
	  && TREE_PURPOSE (TREE_VEC_ELT (parms, nargs)) == NULL_TREE))
    {
      error ("incorrect number of parameters (%d, should be %d)",
	     nargs, nparms);
      if (in_decl)
	cp_error_at ("in template expansion for decl `%D'", in_decl);
      return error_mark_node;
    }

  if (arglist && TREE_CODE (arglist) == TREE_VEC)
    vec = copy_node (arglist);
  else
    {
      vec = make_tree_vec (nparms);
      for (i = 0; i < nparms; i++)
	{
	  tree arg;

	  if (arglist)
	    {
	      arg = arglist;
	      arglist = TREE_CHAIN (arglist);

	      if (arg == error_mark_node)
		lost++;
	      else
		arg = TREE_VALUE (arg);
	    }
	  else
	    arg = TREE_PURPOSE (TREE_VEC_ELT (parms, i));

	  TREE_VEC_ELT (vec, i) = arg;
	}
    }
  for (i = 0; i < nparms; i++)
    {
      tree arg = TREE_VEC_ELT (vec, i);
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      tree val = 0;
      int is_type, requires_type;

      is_type = TREE_CODE_CLASS (TREE_CODE (arg)) == 't';
      requires_type = TREE_CODE (parm) == TYPE_DECL;
      if (is_type != requires_type)
	{
	  if (in_decl)
	    cp_error ("type/value mismatch in template parameter list for `%D'",
		      in_decl);
	  lost++;
	  TREE_VEC_ELT (vec, i) = error_mark_node;
	  continue;
	}
      if (is_type)
	val = groktypename (arg);
      else
	{
	  tree t = tsubst (TREE_TYPE (parm), &TREE_VEC_ELT (vec, 0),
			   TREE_VEC_LENGTH (vec), in_decl);
	  val = digest_init (t, arg, (tree *) 0);

	  if (val == error_mark_node)
	    ;

	  /* 14.2: Other template-arguments must be constant-expressions,
	     addresses of objects or functions with external linkage, or of
	     static class members.  */
	  else if (!TREE_CONSTANT (val))
	    {
	      cp_error ("non-const `%E' cannot be used as template argument",
			arg);
	      val = error_mark_node;
	    }
	  else if (POINTER_TYPE_P (TREE_TYPE (val))
		   && ! integer_zerop (val)
		   && TREE_CODE (TREE_TYPE (TREE_TYPE (val))) != OFFSET_TYPE
		   && TREE_CODE (TREE_TYPE (TREE_TYPE (val))) != METHOD_TYPE)
	    {
	      t = val;
	      STRIP_NOPS (t);
	      if (TREE_CODE (t) == ADDR_EXPR)
		{
		  tree a = TREE_OPERAND (t, 0);
		  STRIP_NOPS (a);
		  if (TREE_CODE (a) == STRING_CST)
		    {
		      cp_error ("string literal %E is not a valid template argument", a);
		      error ("because it is the address of an object with static linkage");
		      val = error_mark_node;
		    }
		  else if (TREE_CODE (a) != VAR_DECL
			   && TREE_CODE (a) != FUNCTION_DECL)
		    goto bad;
		  else if (! DECL_PUBLIC (a))
		    {
		      cp_error ("address of non-extern `%E' cannot be used as template argument", a);
		      val = error_mark_node;
		    }
		}
	      else
		{
		bad:
		  cp_error ("`%E' is not a valid template argument", t);
		  error ("it must be %s%s with external linkage",
			 TREE_CODE (TREE_TYPE (val)) == POINTER_TYPE
			 ? "a pointer to " : "",
			 TREE_CODE (TREE_TYPE (TREE_TYPE (val))) == FUNCTION_TYPE
			 ? "a function" : "an object");
		  val = error_mark_node;
		}
	    }
	}

      if (val == error_mark_node)
	lost++;

      TREE_VEC_ELT (vec, i) = val;
    }
  if (lost)
    return error_mark_node;
  return vec;
}

/* Given class template name and parameter list, produce a user-friendly name
   for the instantiation.  */
static char *
mangle_class_name_for_template (name, parms, arglist)
     char *name;
     tree parms, arglist;
{
  static struct obstack scratch_obstack;
  static char *scratch_firstobj;
  int i, nparms;

  if (!scratch_firstobj)
    {
      gcc_obstack_init (&scratch_obstack);
      scratch_firstobj = obstack_alloc (&scratch_obstack, 1);
    }
  else
    obstack_free (&scratch_obstack, scratch_firstobj);

#if 0
#define buflen	sizeof(buf)
#define check	if (bufp >= buf+buflen-1) goto too_long
#define ccat(c) *bufp++=(c); check
#define advance	bufp+=strlen(bufp); check
#define cat(s)	strncpy(bufp, s, buf+buflen-bufp-1); advance
#else
#define check
#define ccat(c)	obstack_1grow (&scratch_obstack, (c));
#define advance
#define cat(s)	obstack_grow (&scratch_obstack, (s), strlen (s))
#endif

  cat (name);
  ccat ('<');
  nparms = TREE_VEC_LENGTH (parms);
  my_friendly_assert (nparms == TREE_VEC_LENGTH (arglist), 268);
  for (i = 0; i < nparms; i++)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parms, i));
      tree arg = TREE_VEC_ELT (arglist, i);

      if (i)
	ccat (',');

      if (TREE_CODE (parm) == TYPE_DECL)
	{
	  cat (type_as_string (arg, 0));
	  continue;
	}
      else
	my_friendly_assert (TREE_CODE (parm) == PARM_DECL, 269);

      if (TREE_CODE (arg) == TREE_LIST)
	{
	  /* New list cell was built because old chain link was in
	     use.  */
	  my_friendly_assert (TREE_PURPOSE (arg) == NULL_TREE, 270);
	  arg = TREE_VALUE (arg);
	}
      /* No need to check arglist against parmlist here; we did that
	 in coerce_template_parms, called from lookup_template_class.  */
      cat (expr_as_string (arg, 0));
    }
  {
    char *bufp = obstack_next_free (&scratch_obstack);
    int offset = 0;
    while (bufp[offset - 1] == ' ')
      offset--;
    obstack_blank_fast (&scratch_obstack, offset);

    /* B<C<char> >, not B<C<char>> */
    if (bufp[offset - 1] == '>')
      ccat (' ');
  }
  ccat ('>');
  ccat ('\0');
  return (char *) obstack_base (&scratch_obstack);

#if 0
 too_long:
#endif
  fatal ("out of (preallocated) string space creating template instantiation name");
  /* NOTREACHED */
  return NULL;
}

/* Given an IDENTIFIER_NODE (type TEMPLATE_DECL) and a chain of
   parameters, find the desired type.

   D1 is the PTYPENAME terminal, and ARGLIST is the list of arguments.
   Since ARGLIST is build on the decl_obstack, we must copy it here
   to keep it from being reclaimed when the decl storage is reclaimed.

   IN_DECL, if non-NULL, is the template declaration we are trying to
   instantiate.  */
tree
lookup_template_class (d1, arglist, in_decl)
     tree d1, arglist;
     tree in_decl;
{
  tree template, parmlist;
  char *mangled_name;
  tree id;

  my_friendly_assert (TREE_CODE (d1) == IDENTIFIER_NODE, 272);
  template = IDENTIFIER_GLOBAL_VALUE (d1); /* XXX */
  if (! template)
    template = IDENTIFIER_CLASS_VALUE (d1);
  /* With something like `template <class T> class X class X { ... };'
     we could end up with D1 having nothing but an IDENTIFIER_LOCAL_VALUE.
     We don't want to do that, but we have to deal with the situation, so
     let's give them some syntax errors to chew on instead of a crash.  */
  if (! template)
    return error_mark_node;
  if (TREE_CODE (template) != TEMPLATE_DECL)
    {
      cp_error ("non-template type `%T' used as a template", d1);
      if (in_decl)
	cp_error_at ("for template declaration `%D'", in_decl);
      return error_mark_node;
    }
  parmlist = DECL_TEMPLATE_PARMS (template);

  arglist = coerce_template_parms (parmlist, arglist, template);
  if (arglist == error_mark_node)
    return error_mark_node;
  if (uses_template_parms (arglist))
    {
      tree t = make_lang_type (UNINSTANTIATED_P_TYPE);
      tree d;
      id = make_anon_name ();
      d = build_decl (TYPE_DECL, id, t);
      TYPE_NAME (t) = d;
      TYPE_VALUES (t) = build_tree_list (template, arglist);
      pushdecl_top_level (d);
    }
  else
    {
      mangled_name = mangle_class_name_for_template (IDENTIFIER_POINTER (d1),
						     parmlist, arglist);
      id = get_identifier (mangled_name);
    }
  if (!IDENTIFIER_TEMPLATE (id))
    {
      arglist = copy_to_permanent (arglist);
      IDENTIFIER_TEMPLATE (id) = perm_tree_cons (template, arglist, NULL_TREE);
    }
  return id;
}

void
push_template_decls (parmlist, arglist, class_level)
     tree parmlist, arglist;
     int class_level;
{
  int i, nparms;

  /* Don't want to push values into global context.  */
  if (!class_level)
    {
      pushlevel (1);
      declare_pseudo_global_level ();
    }

  nparms = TREE_VEC_LENGTH (parmlist);

  for (i = 0; i < nparms; i++)
    {
      int requires_type, is_type;
      tree parm = TREE_VALUE (TREE_VEC_ELT (parmlist, i));
      tree arg = TREE_VEC_ELT (arglist, i);
      tree decl = 0;

      requires_type = TREE_CODE (parm) == TYPE_DECL;
      is_type = TREE_CODE_CLASS (TREE_CODE (arg)) == 't';
      if (is_type)
	{
	  /* add typename to namespace */
	  if (!requires_type)
	    {
	      error ("template use error: type provided where value needed");
	      continue;
	    }
	  decl = arg;
	  my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (decl)) == 't', 273);
	  decl = build_decl (TYPE_DECL, DECL_NAME (parm), decl);
	}
      else
	{
	  /* add const decl to namespace */
	  tree val;
	  tree parmtype;
	  if (requires_type)
	    {
	      error ("template use error: value provided where type needed");
	      continue;
	    }
	  parmtype = tsubst (TREE_TYPE (parm), &TREE_VEC_ELT (arglist, 0),
			     TREE_VEC_LENGTH (arglist), NULL_TREE);
	  val = digest_init (parmtype, arg, (tree *) 0);
	  if (val != error_mark_node)
	    {
	      decl = build_decl (CONST_DECL, DECL_NAME (parm),
				 parmtype);
	      DECL_INITIAL (decl) = val;
	      TREE_READONLY (decl) = 1;
	    }
	}
      if (decl != 0)
	{
	  SET_DECL_ARTIFICIAL (decl);
	  layout_decl (decl, 0);
	  if (class_level)
	    pushdecl_class_level (decl);
	  else
	    pushdecl (decl);
	}
    }
}

void
pop_template_decls (parmlist, arglist, class_level)
     tree parmlist, arglist;
     int class_level;
{
  if (!class_level)
    poplevel (0, 0, 0);
}

/* Should be defined in parse.h.  */
extern int yychar;

int
uses_template_parms (t)
     tree t;
{
  if (!t)
    return 0;
  switch (TREE_CODE (t))
    {
    case INDIRECT_REF:
    case COMPONENT_REF:
      /* We assume that the object must be instantiated in order to build
	 the COMPONENT_REF, so we test only whether the type of the
	 COMPONENT_REF uses template parms.  */
      return uses_template_parms (TREE_TYPE (t));

    case IDENTIFIER_NODE:
      if (!IDENTIFIER_TEMPLATE (t))
	return 0;
      return uses_template_parms (TREE_VALUE (IDENTIFIER_TEMPLATE (t)));

      /* aggregates of tree nodes */
    case TREE_VEC:
      {
	int i = TREE_VEC_LENGTH (t);
	while (i--)
	  if (uses_template_parms (TREE_VEC_ELT (t, i)))
	    return 1;
	return 0;
      }
    case TREE_LIST:
      if (uses_template_parms (TREE_PURPOSE (t))
	  || uses_template_parms (TREE_VALUE (t)))
	return 1;
      return uses_template_parms (TREE_CHAIN (t));

      /* constructed type nodes */
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      return uses_template_parms (TREE_TYPE (t));
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_FLAG (t))
	return uses_template_parms (TYPE_PTRMEMFUNC_FN_TYPE (t));
    case UNION_TYPE:
      if (!TYPE_NAME (t))
	return 0;
      if (!TYPE_IDENTIFIER (t))
	return 0;
      return uses_template_parms (TYPE_IDENTIFIER (t));
    case FUNCTION_TYPE:
      if (uses_template_parms (TYPE_ARG_TYPES (t)))
	return 1;
      return uses_template_parms (TREE_TYPE (t));
    case ARRAY_TYPE:
      if (uses_template_parms (TYPE_DOMAIN (t)))
	return 1;
      return uses_template_parms (TREE_TYPE (t));
    case OFFSET_TYPE:
      if (uses_template_parms (TYPE_OFFSET_BASETYPE (t)))
	return 1;
      return uses_template_parms (TREE_TYPE (t));
    case METHOD_TYPE:
      if (uses_template_parms (TYPE_METHOD_BASETYPE (t)))
	return 1;
      if (uses_template_parms (TYPE_ARG_TYPES (t)))
	return 1;
      return uses_template_parms (TREE_TYPE (t));

      /* decl nodes */
    case TYPE_DECL:
      return uses_template_parms (DECL_NAME (t));
    case FUNCTION_DECL:
      if (uses_template_parms (TREE_TYPE (t)))
	return 1;
      /* fall through */
    case VAR_DECL:
    case PARM_DECL:
      /* ??? What about FIELD_DECLs?  */
      /* The type of a decl can't use template parms if the name of the
	 variable doesn't, because it's impossible to resolve them.  So
	 ignore the type field for now.	 */
      if (DECL_CONTEXT (t) && uses_template_parms (DECL_CONTEXT (t)))
	return 1;
      if (uses_template_parms (TREE_TYPE (t)))
	{
	  error ("template parms used where they can't be resolved");
	}
      return 0;

    case CALL_EXPR:
      return uses_template_parms (TREE_TYPE (t));
    case ADDR_EXPR:
      return uses_template_parms (TREE_OPERAND (t, 0));

      /* template parm nodes */
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_CONST_PARM:
      return 1;

      /* simple type nodes */
    case INTEGER_TYPE:
      if (uses_template_parms (TYPE_MIN_VALUE (t)))
	return 1;
      return uses_template_parms (TYPE_MAX_VALUE (t));

    case REAL_TYPE:
    case VOID_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      return 0;

      /* constants */
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      return 0;

    case ERROR_MARK:
      /* Non-error_mark_node ERROR_MARKs are bad things.  */
      my_friendly_assert (t == error_mark_node, 274);
      /* NOTREACHED */
      return 0;

    case UNINSTANTIATED_P_TYPE:
      return 1;

    case CONSTRUCTOR:
      if (TREE_TYPE (t) && TYPE_PTRMEMFUNC_P (TREE_TYPE (t)))
	return uses_template_parms (TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (t)));
      /* else fall through */

    default:
      switch (TREE_CODE_CLASS (TREE_CODE (t)))
	{
	case '1':
	case '2':
	case '3':
	case '<':
	  {
	    int i;
	    for (i = tree_code_length[(int) TREE_CODE (t)]; --i >= 0;)
	      if (uses_template_parms (TREE_OPERAND (t, i)))
		return 1;
	    return 0;
	  }
	default:
	  break;
	}
      sorry ("testing %s for template parms",
	     tree_code_name [(int) TREE_CODE (t)]);
      my_friendly_abort (82);
      /* NOTREACHED */
      return 0;
    }
}

void
instantiate_member_templates (classname)
     tree classname;
{
  tree t;
  tree id = classname;
  tree members = DECL_TEMPLATE_MEMBERS (TREE_PURPOSE (IDENTIFIER_TEMPLATE (id)));

  for (t = members; t; t = TREE_CHAIN (t))
    {
      tree parmvec, type, classparms, tdecl, t2;
      int nparms, xxx = 0, i;

      my_friendly_assert (TREE_VALUE (t) != NULL_TREE, 275);
      my_friendly_assert (TREE_CODE (TREE_VALUE (t)) == TEMPLATE_DECL, 276);
      /* @@ Should verify that class parm list is a list of
	 distinct template parameters, and covers all the template
	 parameters.  */
      tdecl = TREE_VALUE (t);
      type = DECL_CONTEXT (DECL_TEMPLATE_RESULT (tdecl));
      classparms = UPT_PARMS (type);
      nparms = TREE_VEC_LENGTH (classparms);
      parmvec = make_tree_vec (nparms);
      for (i = 0; i < nparms; i++)
	TREE_VEC_ELT (parmvec, i) = NULL_TREE;
      switch (unify (DECL_TEMPLATE_PARMS (tdecl),
		     &TREE_VEC_ELT (parmvec, 0), nparms,
		     type, IDENTIFIER_TYPE_VALUE (classname),
		     &xxx))
	{
	case 0:
	  /* Success -- well, no inconsistency, at least.  */
	  for (i = 0; i < nparms; i++)
	    if (TREE_VEC_ELT (parmvec, i) == NULL_TREE)
	      goto failure;
	  t2 = instantiate_template (tdecl,
				     &TREE_VEC_ELT (parmvec, 0));
	  type = IDENTIFIER_TYPE_VALUE (id);
	  my_friendly_assert (type != 0, 277);
	  break;
	case 1:
	  /* Failure.  */
	failure:
	  cp_error_at ("type unification error instantiating `%D'", tdecl);
	  cp_error ("while instantiating members of `%T'", classname);

	  continue /* loop of members */;
	default:
	  /* Eek, a bug.  */
	  my_friendly_abort (83);
	}
    }
}

static struct tinst_level *current_tinst_level = 0;
static struct tinst_level *free_tinst_level = 0;
static int tinst_depth = 0;
int max_tinst_depth = 17;

int
push_tinst_level (name)
     tree name;
{
  struct tinst_level *new;
  tree global = IDENTIFIER_GLOBAL_VALUE (name);

  if (tinst_depth >= max_tinst_depth)
    {
      error ("template instantiation depth exceeds maximum of %d",
	     max_tinst_depth);
      cp_error ("  instantiating `%D'", name);
      return 0;
    }

  if (free_tinst_level)
    {
      new = free_tinst_level;
      free_tinst_level = new->next;
    }
  else
    new = (struct tinst_level *) xmalloc (sizeof (struct tinst_level));

  new->classname = name;
  if (global)
    {
      new->line = DECL_SOURCE_LINE (global);
      new->file = DECL_SOURCE_FILE (global);
    }
  else
    {
      new->line = lineno;
      new->file = input_filename;
    }
  new->next = current_tinst_level;
  current_tinst_level = new;
  ++tinst_depth;
  return 1;
}

void
pop_tinst_level ()
{
  struct tinst_level *old = current_tinst_level;

  current_tinst_level = old->next;
  old->next = free_tinst_level;
  free_tinst_level = old;
  --tinst_depth;
}

struct tinst_level *
tinst_for_decl ()
{
  struct tinst_level *p = current_tinst_level;

  if (p)
    for (; p->next ; p = p->next )
      ;
  return p;
}

tree
instantiate_class_template (classname, setup_parse)
     tree classname;
     int setup_parse;
{
  struct template_info *template_info;
  tree template, t1;

  if (classname == error_mark_node)
    return error_mark_node;

  my_friendly_assert (TREE_CODE (classname) == IDENTIFIER_NODE, 278);
  template = IDENTIFIER_TEMPLATE (classname);

  if (IDENTIFIER_HAS_TYPE_VALUE (classname))
    {
      tree type = IDENTIFIER_TYPE_VALUE (classname);
      if (TREE_CODE (type) == UNINSTANTIATED_P_TYPE)
	return type;
      if (TYPE_BEING_DEFINED (type)
	  || TYPE_SIZE (type)
	  || CLASSTYPE_USE_TEMPLATE (type) != 0)
	return type;
    }

  /* If IDENTIFIER_LOCAL_VALUE is already set on this template classname
     (it's something like `foo<int>'), that means we're already working on
     the instantiation for it.  Normally, a classname comes in with nothing
     but its IDENTIFIER_TEMPLATE slot set.  If we were to try to instantiate
     this again, we'd get a redeclaration error.  Since we're already working
     on it, we'll pass back this classname's TYPE_DECL (it's the value of
     the classname's IDENTIFIER_LOCAL_VALUE).  Only do this if we're setting
     things up for the parser, though---if we're just trying to instantiate
     it (e.g., via tsubst) we can trip up cuz it may not have an
     IDENTIFIER_TYPE_VALUE when it will need one.  */
  if (setup_parse && IDENTIFIER_LOCAL_VALUE (classname))
    return IDENTIFIER_LOCAL_VALUE (classname);

  if (uses_template_parms (classname))
    {
      if (!TREE_TYPE (classname))
	{
	  tree t = make_lang_type (RECORD_TYPE);
	  tree d = build_decl (TYPE_DECL, classname, t);
	  DECL_NAME (d) = classname;
	  TYPE_NAME (t) = d;
	  pushdecl (d);
	}
      return NULL_TREE;
    }

  t1 = TREE_PURPOSE (template);
  my_friendly_assert (TREE_CODE (t1) == TEMPLATE_DECL, 279);

  /* If a template is declared but not defined, accept it; don't crash.
     Later uses requiring the definition will be flagged as errors by
     other code.  Thanks to niklas@appli.se for this bug fix.  */
  if (DECL_TEMPLATE_INFO (t1)->text == 0)
    setup_parse = 0;

  push_to_top_level ();
  template_info = DECL_TEMPLATE_INFO (t1);
  if (setup_parse && push_tinst_level (classname))
    {
      push_template_decls (DECL_TEMPLATE_PARMS (TREE_PURPOSE (template)),
			   TREE_VALUE (template), 0);
      set_current_level_tags_transparency (1);
      feed_input (template_info->text, template_info->length, (struct obstack *)0);
      lineno = template_info->lineno;
      input_filename = template_info->filename;
      /* Get interface/implementation back in sync.  */
      extract_interface_info ();
      overload_template_name (classname, 0);
      /* Kludge so that we don't get screwed by our own base classes.  */
      TYPE_BEING_DEFINED (TREE_TYPE (classname)) = 1;
      yychar = PRE_PARSED_CLASS_DECL;
      yylval.ttype = classname;
      processing_template_defn++;
      if (!flag_external_templates)
	interface_unknown++;
      template_classes
	= perm_tree_cons (classname, NULL_TREE, template_classes);
    }
  else
    {
      tree t, decl, id, tmpl;

      id = classname;
      tmpl = TREE_PURPOSE (IDENTIFIER_TEMPLATE (id));
      t = xref_tag (DECL_TEMPLATE_INFO (tmpl)->aggr, id, NULL_TREE, 0);
      my_friendly_assert (TREE_CODE (t) == RECORD_TYPE
			  || TREE_CODE (t) == UNION_TYPE, 280);

      /* Now, put a copy of the decl in global scope, to avoid
       * recursive expansion.  */
      decl = IDENTIFIER_LOCAL_VALUE (id);
      if (!decl)
	decl = IDENTIFIER_CLASS_VALUE (id);
      if (decl)
	{
	  my_friendly_assert (TREE_CODE (decl) == TYPE_DECL, 281);
	  /* We'd better make sure we're on the permanent obstack or else
	   * we'll get a "friendly" abort 124 in pushdecl.  Perhaps a
	   * copy_to_permanent would be sufficient here, but then a
	   * sharing problem might occur.  I don't know -- niklas@appli.se */
	  push_obstacks (&permanent_obstack, &permanent_obstack);
	  pushdecl_top_level (copy_node (decl));
	  pop_obstacks ();
	}
      pop_from_top_level ();
    }

  return NULL_TREE;
}

static int
list_eq (t1, t2)
     tree t1, t2;
{
  if (t1 == NULL_TREE)
    return t2 == NULL_TREE;
  if (t2 == NULL_TREE)
    return 0;
  /* Don't care if one declares its arg const and the other doesn't -- the
     main variant of the arg type is all that matters.  */
  if (TYPE_MAIN_VARIANT (TREE_VALUE (t1))
      != TYPE_MAIN_VARIANT (TREE_VALUE (t2)))
    return 0;
  return list_eq (TREE_CHAIN (t1), TREE_CHAIN (t2));
}

static tree 
lookup_nested_type_by_name (ctype, name)
        tree ctype, name;
{
  tree t;

  for (t = CLASSTYPE_TAGS (ctype); t; t = TREE_CHAIN (t))
    {
      if (name == TREE_PURPOSE (t))
	return TREE_VALUE (t);
    }
  return NULL_TREE;
}

static tree
search_nested_type_in_tmpl (tmpl, type)
        tree tmpl, type;
{
  tree t;

  if (tmpl == NULL || TYPE_CONTEXT(type) == NULL)
    return tmpl;
  t = search_nested_type_in_tmpl (tmpl, TYPE_CONTEXT(type));
  if (t == NULL) return t;
  t = lookup_nested_type_by_name(t, DECL_NAME(TYPE_NAME(type)));
  return t;
}

tree
tsubst (t, args, nargs, in_decl)
     tree t, *args;
     int nargs;
     tree in_decl;
{
  tree type;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  type = TREE_TYPE (t);
  if (type
      /* Minor optimization.
	 ?? Are these really the most frequent cases?  Is the savings
	 significant?  */
      && type != integer_type_node
      && type != void_type_node
      && type != char_type_node)
    type = tsubst (type, args, nargs, in_decl);

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	return build_ptrmemfunc_type
	  (tsubst (TYPE_PTRMEMFUNC_FN_TYPE (t), args, nargs, in_decl));
	  
      /* else fall through */

    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case OP_IDENTIFIER:
    case VOID_TYPE:
    case REAL_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case UNION_TYPE:
      return t;

    case INTEGER_TYPE:
      if (t == integer_type_node)
	return t;

      if (TREE_CODE (TYPE_MIN_VALUE (t)) == INTEGER_CST
	  && TREE_CODE (TYPE_MAX_VALUE (t)) == INTEGER_CST)
	return t;
      return build_index_2_type
	(tsubst (TYPE_MIN_VALUE (t), args, nargs, in_decl),
	 tsubst (TYPE_MAX_VALUE (t), args, nargs, in_decl));

    case TEMPLATE_TYPE_PARM:
      {
	tree arg = args[TEMPLATE_TYPE_IDX (t)];
	return cp_build_type_variant
	  (arg, TYPE_READONLY (arg) || TYPE_READONLY (t),
	   TYPE_VOLATILE (arg) || TYPE_VOLATILE (t));
      }

    case TEMPLATE_CONST_PARM:
      return args[TEMPLATE_CONST_IDX (t)];

    case FUNCTION_DECL:
      {
	tree r;
	tree fnargs, result;
	
	if (type == TREE_TYPE (t)
	    && (DECL_CONTEXT (t) == NULL_TREE
		|| TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (t))) != 't'))
	  return t;
	fnargs = tsubst (DECL_ARGUMENTS (t), args, nargs, t);
	result = tsubst (DECL_RESULT (t), args, nargs, t);
	if (DECL_CONTEXT (t) != NULL_TREE
	    && TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (t))) == 't')
	  {
	    /* Look it up in that class, and return the decl node there,
	       instead of creating a new one.  */
	    tree ctx, methods, name, method;
	    int n_methods;
	    int i, found = 0;

	    name = DECL_NAME (t);
	    ctx = tsubst (DECL_CONTEXT (t), args, nargs, t);
	    methods = CLASSTYPE_METHOD_VEC (ctx);
	    if (methods == NULL_TREE)
	      /* No methods at all -- no way this one can match.  */
	      goto no_match;
	    n_methods = TREE_VEC_LENGTH (methods);

	    r = NULL_TREE;

	    if (!strncmp (OPERATOR_TYPENAME_FORMAT,
			  IDENTIFIER_POINTER (name),
			  sizeof (OPERATOR_TYPENAME_FORMAT) - 1))
	      {
		/* Type-conversion operator.  Reconstruct the name, in
		   case it's the name of one of the template's parameters.  */
		name = build_typename_overload (TREE_TYPE (type));
	      }

	    if (DECL_CONTEXT (t) != NULL_TREE
		&& TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (t))) == 't'
		&& constructor_name (DECL_CONTEXT (t)) == DECL_NAME (t))
	      name = constructor_name (ctx);

	    if (DECL_CONSTRUCTOR_P (t) && TYPE_USES_VIRTUAL_BASECLASSES (ctx))
	      {
		/* Since we didn't know that this class had virtual bases until after
		   we instantiated it, we have to recreate the arguments to this
		   constructor, as otherwise it would miss the __in_chrg parameter.  */
		tree newtype, parm;
		tree parms = TREE_CHAIN (TYPE_ARG_TYPES (type));
		parms = hash_tree_chain (integer_type_node, parms);
		newtype = build_cplus_method_type (ctx,
						   TREE_TYPE (type),
						   parms);
		newtype = build_type_variant (newtype,
					      TYPE_READONLY (type),
					      TYPE_VOLATILE (type));
		type = newtype;

		fnargs = copy_node (DECL_ARGUMENTS (t));
		TREE_CHAIN (fnargs) = TREE_CHAIN (DECL_ARGUMENTS (t));

		/* In this case we need "in-charge" flag saying whether
		   this constructor is responsible for initialization
		   of virtual baseclasses or not.  */
		parm = build_decl (PARM_DECL, in_charge_identifier, integer_type_node);
		/* Mark the artificial `__in_chrg' parameter as "artificial".  */
		SET_DECL_ARTIFICIAL (parm);
		DECL_ARG_TYPE (parm) = integer_type_node;
		DECL_REGISTER (parm) = 1;
		TREE_CHAIN (parm) = TREE_CHAIN (fnargs);
		TREE_CHAIN (fnargs) = parm;

		fnargs = tsubst (fnargs, args, nargs, t);
	      }
#if 0
	    fprintf (stderr, "\nfor function %s in class %s:\n",
		     IDENTIFIER_POINTER (name),
		     IDENTIFIER_POINTER (TYPE_IDENTIFIER (ctx)));
#endif
	    for (i = 0; i < n_methods; i++)
	      {
		int pass;

		method = TREE_VEC_ELT (methods, i);
		if (method == NULL_TREE || DECL_NAME (method) != name)
		  continue;

		pass = 0;
	      maybe_error:
		for (; method; method = DECL_CHAIN (method))
		  {
		    my_friendly_assert (TREE_CODE (method) == FUNCTION_DECL,
					282);
		    if (! comptypes (type, TREE_TYPE (method), 1))
		      {
			tree mtype = TREE_TYPE (method);
			tree t1, t2;

			/* Keep looking for a method that matches
			   perfectly.  This takes care of the problem
			   where destructors (which have implicit int args)
			   look like constructors which have an int arg.  */
			if (pass == 0)
			  continue;

			t1 = TYPE_ARG_TYPES (mtype);
			t2 = TYPE_ARG_TYPES (type);
			if (TREE_CODE (mtype) == FUNCTION_TYPE)
			  t2 = TREE_CHAIN (t2);

			if (list_eq (t1, t2))
			  {
			    if (TREE_CODE (mtype) == FUNCTION_TYPE)
			      {
				tree newtype;
				newtype = build_function_type (TREE_TYPE (type),
							       TYPE_ARG_TYPES (type));
				newtype = build_type_variant (newtype,
							      TYPE_READONLY (type),
							      TYPE_VOLATILE (type));
				type = newtype;
				if (TREE_TYPE (type) != TREE_TYPE (mtype))
				  goto maybe_bad_return_type;
			      }
			    else if (TYPE_METHOD_BASETYPE (mtype)
				     == TYPE_METHOD_BASETYPE (type))
			      {
				/* Types didn't match, but arg types and
				   `this' do match, so the return type is
				   all that should be messing it up.  */
			      maybe_bad_return_type:
				if (TREE_TYPE (type) != TREE_TYPE (mtype))
				  error ("inconsistent return types for method `%s' in class `%s'",
					 IDENTIFIER_POINTER (name),
					 IDENTIFIER_POINTER (TYPE_IDENTIFIER (ctx)));
			      }
			    r = method;
			    break;
			  }
			found = 1;
			continue;
		      }
#if 0
		    fprintf (stderr, "\tfound %s\n\n",
			     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (method)));
#endif
		    if (DECL_ARTIFICIAL (method))
		      {
			cp_error ("template for method `%D' which has default implementation in class `%T'", name, ctx);
			if (in_decl)
			  cp_error_at ("in attempt to instantiate `%D' declared at this point in file", in_decl);
			return error_mark_node;
		      }

		    if (DECL_ARGUMENTS (method)
			&& ! TREE_PERMANENT (DECL_ARGUMENTS (method)))
		      /* @@ Is this early enough?  Might we want to do
			 this instead while processing the expansion?	 */
		      DECL_ARGUMENTS (method)
			= tsubst (DECL_ARGUMENTS (t), args, nargs, t);
		    r = method;
		    break;
		  }
		if (r == NULL_TREE && pass == 0)
		  {
		    pass = 1;
		    method = TREE_VEC_ELT (methods, i);
		    goto maybe_error;
		  }
	      }
	    if (r == NULL_TREE)
	      {
	      no_match:
		cp_error
		  (found
		   ? "template for method `%D' doesn't match any in class `%T'"
		   : "method `%D' not found in class `%T'", name, ctx);
		if (in_decl)
		  cp_error_at ("in attempt to instantiate `%D' declared at this point in file", in_decl);
		return error_mark_node;
	      }
	  }
	else
	  {
	    r = DECL_NAME (t);
	    {
	      tree decls;
	      int got_it = 0;

	      decls = lookup_name_nonclass (r);
	      if (decls == NULL_TREE)
		/* no match */;
	      else if (TREE_CODE (decls) == TREE_LIST)
		for (decls = TREE_VALUE (decls); decls ;
		     decls = DECL_CHAIN (decls))
		  {
		    if (TREE_CODE (decls) == FUNCTION_DECL
			&& TREE_TYPE (decls) == type)
		      {
			got_it = 1;
			r = decls;
			break;
		      }
		  }
	      else
		{
		  tree val = decls;
		  decls = NULL_TREE;
		  if (TREE_CODE (val) == FUNCTION_DECL
		      && TREE_TYPE (val) == type)
		    {
		      got_it = 1;
		      r = val;
		    }
		}

	      if (!got_it)
		{
		  tree a = build_decl_overload (r, TYPE_VALUES (type),
						DECL_CONTEXT (t) != NULL_TREE);
		  r = build_lang_decl (FUNCTION_DECL, r, type);
		  DECL_ASSEMBLER_NAME (r) = a;
		}
	      else if (TREE_STATIC (r))
		{
		  /* This overrides the template version, use it. */
		  return r;
		}
	    }
	  }
	TREE_PUBLIC (r) = 1;
	DECL_EXTERNAL (r) = 1;
	TREE_STATIC (r) = 0;
	DECL_INTERFACE_KNOWN (r) = 0;
	DECL_INLINE (r) = DECL_INLINE (t);
	DECL_THIS_INLINE (r) = DECL_THIS_INLINE (t);
	TREE_READONLY (r) = TREE_READONLY (t);
	TREE_THIS_VOLATILE (r) = TREE_THIS_VOLATILE (t);
	{
#if 0				/* Maybe later.  -jason  */
	  struct tinst_level *til = tinst_for_decl();

	  /* should always be true under new approach */
	  if (til)
	    {
	      DECL_SOURCE_FILE (r) = til->file;
	      DECL_SOURCE_LINE (r) = til->line;
	    }
	  else
#endif
	    {
	      DECL_SOURCE_FILE (r) = DECL_SOURCE_FILE (t);
	      DECL_SOURCE_LINE (r) = DECL_SOURCE_LINE (t);
	    }
	}
	DECL_CLASS_CONTEXT (r) = tsubst (DECL_CLASS_CONTEXT (t), args, nargs, t);
	make_decl_rtl (r, NULL_PTR, 1);
	DECL_ARGUMENTS (r) = fnargs;
	DECL_RESULT (r) = result;
#if 0
	if (DECL_CONTEXT (t) == NULL_TREE
	    || TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (t))) != 't')
	  push_overloaded_decl_top_level (r, 0);
#endif
	return r;
      }

    case PARM_DECL:
      {
	tree r;
	r = build_decl (PARM_DECL, DECL_NAME (t), type);
	DECL_INITIAL (r) = TREE_TYPE (r);
	DECL_ARTIFICIAL (r) = DECL_ARTIFICIAL (t);
#ifdef PROMOTE_PROTOTYPES
	if ((TREE_CODE (type) == INTEGER_TYPE
	     || TREE_CODE (type) == ENUMERAL_TYPE)
	    && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	  DECL_ARG_TYPE (r) = integer_type_node;
#endif
	if (TREE_CHAIN (t))
	  TREE_CHAIN (r) = tsubst (TREE_CHAIN (t), args, nargs, TREE_CHAIN (t));
	return r;
      }

    case TREE_LIST:
      {
	tree purpose, value, chain, result;
	int via_public, via_virtual, via_protected;

	if (t == void_list_node)
	  return t;

	via_public = TREE_VIA_PUBLIC (t);
	via_protected = TREE_VIA_PROTECTED (t);
	via_virtual = TREE_VIA_VIRTUAL (t);

	purpose = TREE_PURPOSE (t);
	if (purpose)
	  purpose = tsubst (purpose, args, nargs, in_decl);
	value = TREE_VALUE (t);
	if (value)
	  value = tsubst (value, args, nargs, in_decl);
	chain = TREE_CHAIN (t);
	if (chain && chain != void_type_node)
	  chain = tsubst (chain, args, nargs, in_decl);
	if (purpose == TREE_PURPOSE (t)
	    && value == TREE_VALUE (t)
	    && chain == TREE_CHAIN (t))
	  return t;
	result = hash_tree_cons (via_public, via_virtual, via_protected,
				 purpose, value, chain);
	TREE_PARMLIST (result) = TREE_PARMLIST (t);
	return result;
      }
    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (t), need_new = 0, i;
	tree *elts = (tree *) alloca (len * sizeof (tree));
	bzero ((char *) elts, len * sizeof (tree));

	for (i = 0; i < len; i++)
	  {
	    elts[i] = tsubst (TREE_VEC_ELT (t, i), args, nargs, in_decl);
	    if (elts[i] != TREE_VEC_ELT (t, i))
	      need_new = 1;
	  }

	if (!need_new)
	  return t;

	t = make_tree_vec (len);
	for (i = 0; i < len; i++)
	  TREE_VEC_ELT (t, i) = elts[i];
	return t;
      }
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	tree r;
	enum tree_code code;
	if (type == TREE_TYPE (t))
	  return t;

	code = TREE_CODE (t);
	if (code == POINTER_TYPE)
	  r = build_pointer_type (type);
	else
	  r = build_reference_type (type);
	r = cp_build_type_variant (r, TYPE_READONLY (t), TYPE_VOLATILE (t));
	/* Will this ever be needed for TYPE_..._TO values?  */
	layout_type (r);
	return r;
      }
    case OFFSET_TYPE:
      return build_offset_type
	(tsubst (TYPE_OFFSET_BASETYPE (t), args, nargs, in_decl), type);
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree values = TYPE_ARG_TYPES (t);
	tree context = TYPE_CONTEXT (t);
	tree new_value;

	/* Don't bother recursing if we know it won't change anything.	*/
	if (values != void_list_node)
	  values = tsubst (values, args, nargs, in_decl);
	if (context)
	  context = tsubst (context, args, nargs, in_decl);
	/* Could also optimize cases where return value and
	   values have common elements (e.g., T min(const &T, const T&).  */

	/* If the above parameters haven't changed, just return the type.  */
	if (type == TREE_TYPE (t)
	    && values == TYPE_VALUES (t)
	    && context == TYPE_CONTEXT (t))
	  return t;

	/* Construct a new type node and return it.  */
	if (TREE_CODE (t) == FUNCTION_TYPE
	    && context == NULL_TREE)
	  {
	    new_value = build_function_type (type, values);
	  }
	else if (context == NULL_TREE)
	  {
	    tree base = tsubst (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t))),
				args, nargs, in_decl);
	    new_value = build_cplus_method_type (base, type,
						 TREE_CHAIN (values));
	  }
	else
	  {
	    new_value = make_node (TREE_CODE (t));
	    TREE_TYPE (new_value) = type;
	    TYPE_CONTEXT (new_value) = context;
	    TYPE_VALUES (new_value) = values;
	    TYPE_SIZE (new_value) = TYPE_SIZE (t);
	    TYPE_ALIGN (new_value) = TYPE_ALIGN (t);
	    TYPE_MODE (new_value) = TYPE_MODE (t);
	    if (TYPE_METHOD_BASETYPE (t))
	      TYPE_METHOD_BASETYPE (new_value) = tsubst (TYPE_METHOD_BASETYPE (t),
							 args, nargs, in_decl);
	    /* Need to generate hash value.  */
	    my_friendly_abort (84);
	  }
	new_value = build_type_variant (new_value,
					TYPE_READONLY (t),
					TYPE_VOLATILE (t));
	return new_value;
      }
    case ARRAY_TYPE:
      {
	tree domain = tsubst (TYPE_DOMAIN (t), args, nargs, in_decl);
	tree r;
	if (type == TREE_TYPE (t) && domain == TYPE_DOMAIN (t))
	  return t;
	r = build_cplus_array_type (type, domain);
	return r;
      }

    case UNINSTANTIATED_P_TYPE:
      {
	int nparms = TREE_VEC_LENGTH (DECL_TEMPLATE_PARMS (UPT_TEMPLATE (t)));
	tree argvec = make_tree_vec (nparms);
	tree parmvec = UPT_PARMS (t);
	int i;
	tree id, rt;
	for (i = 0; i < nparms; i++)
	  TREE_VEC_ELT (argvec, i) = tsubst (TREE_VEC_ELT (parmvec, i),
					     args, nargs, in_decl);
	id = lookup_template_class (DECL_NAME (UPT_TEMPLATE (t)), argvec, NULL_TREE);
	if (! IDENTIFIER_HAS_TYPE_VALUE (id)) {
	  instantiate_class_template(id, 0);
	  /* set up pending_classes */
	  add_pending_template (id);

	  TYPE_MAIN_VARIANT (IDENTIFIER_TYPE_VALUE (id)) =
	    IDENTIFIER_TYPE_VALUE (id);
	}
        rt = IDENTIFIER_TYPE_VALUE (id);

	/* kung: this part handles nested type in template definition */
        
	if ( !ANON_AGGRNAME_P (DECL_NAME(TYPE_NAME(t))))
          {
	    rt = search_nested_type_in_tmpl (rt, t);
          }

	return build_type_variant (rt, TYPE_READONLY (t), TYPE_VOLATILE (t));
      }

    case MINUS_EXPR:
    case PLUS_EXPR:
      return fold (build (TREE_CODE (t), TREE_TYPE (t),
			  tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl),
			  tsubst (TREE_OPERAND (t, 1), args, nargs, in_decl)));

    case NEGATE_EXPR:
    case NOP_EXPR:
      return fold (build1 (TREE_CODE (t), TREE_TYPE (t),
			   tsubst (TREE_OPERAND (t, 0), args, nargs, in_decl)));

    default:
      sorry ("use of `%s' in function template",
	     tree_code_name [(int) TREE_CODE (t)]);
      return error_mark_node;
    }
}

tree
instantiate_template (tmpl, targ_ptr)
     tree tmpl, *targ_ptr;
{
  tree targs, fndecl;
  int i, len;
  struct pending_inline *p;
  struct template_info *t;
  struct obstack *old_fmp_obstack;
  extern struct obstack *function_maybepermanent_obstack;

  push_obstacks (&permanent_obstack, &permanent_obstack);
  old_fmp_obstack = function_maybepermanent_obstack;
  function_maybepermanent_obstack = &permanent_obstack;

  my_friendly_assert (TREE_CODE (tmpl) == TEMPLATE_DECL, 283);
  len = TREE_VEC_LENGTH (DECL_TEMPLATE_PARMS (tmpl));

  i = len;
  while (i--)
    targ_ptr[i] = copy_to_permanent (targ_ptr[i]);

  for (fndecl = DECL_TEMPLATE_INSTANTIATIONS (tmpl);
       fndecl; fndecl = TREE_CHAIN (fndecl))
    {
      tree *t1 = &TREE_VEC_ELT (TREE_PURPOSE (fndecl), 0);
      for (i = len - 1; i >= 0; i--)
	if (simple_cst_equal (t1[i], targ_ptr[i]) <= 0)
	  goto no_match;

      /* Here, we have a match.  */
      fndecl = TREE_VALUE (fndecl);
      goto exit;

    no_match:
      ;
    }

  targs = make_tree_vec (len);
  i = len;
  while (i--)
    TREE_VEC_ELT (targs, i) = targ_ptr[i];

  /* substitute template parameters */
  fndecl = tsubst (DECL_RESULT (tmpl), targ_ptr,
		   TREE_VEC_LENGTH (targs), tmpl);

  if (fndecl == error_mark_node)
    goto exit;

  assemble_external (fndecl);

  /* If it's a static member fn in the template, we need to change it
     into a FUNCTION_TYPE and chop off its this pointer.  */
  if (TREE_CODE (TREE_TYPE (DECL_RESULT (tmpl))) == METHOD_TYPE
      && DECL_STATIC_FUNCTION_P (fndecl))
    {
      revert_static_member_fn (&DECL_RESULT (tmpl), NULL, NULL);
      /* Chop off the this pointer that grokclassfn so kindly added
	 for us (it didn't know yet if the fn was static or not).  */
      DECL_ARGUMENTS (fndecl) = TREE_CHAIN (DECL_ARGUMENTS (fndecl));
    }
     
  t = DECL_TEMPLATE_INFO (tmpl);

  /* If we have a preexisting version of this function, don't expand
     the template version, use the other instead.  */
  if (TREE_STATIC (fndecl) || DECL_TEMPLATE_SPECIALIZATION (fndecl))
    {
      SET_DECL_TEMPLATE_SPECIALIZATION (fndecl);
      p = (struct pending_inline *)0;
    }
  else if (t->text)
    {
      SET_DECL_IMPLICIT_INSTANTIATION (fndecl);
      repo_template_used (fndecl);
      p = (struct pending_inline *) permalloc (sizeof (struct pending_inline));
      p->parm_vec = t->parm_vec;
      p->bindings = targs;
      p->can_free = 0;
      p->deja_vu = 0;
      p->buf = t->text;
      p->len = t->length;
      p->fndecl = fndecl;
      {
	int l = lineno;
	char * f = input_filename;

	lineno = p->lineno = t->lineno;
	input_filename = p->filename = t->filename;

	extract_interface_info ();

	if (interface_unknown && flag_external_templates)
	  {
	    if (DECL_CLASS_CONTEXT (fndecl)
		&& CLASSTYPE_INTERFACE_KNOWN (DECL_CLASS_CONTEXT (fndecl)))
	      {
		interface_unknown = 0;
		interface_only
		  = CLASSTYPE_INTERFACE_ONLY (DECL_CLASS_CONTEXT (fndecl));
	      }
	    else if (! DECL_IN_SYSTEM_HEADER (tmpl))
	      warn_if_unknown_interface (tmpl);
	  }

	if (interface_unknown || ! flag_external_templates)
	  p->interface = 1;		/* unknown */
	else
	  p->interface = interface_only ? 0 : 2;

	lineno = l;
	input_filename = f;

	extract_interface_info ();
      }
    }
  else
    p = (struct pending_inline *)0;

  DECL_TEMPLATE_INSTANTIATIONS (tmpl) =
    tree_cons (targs, fndecl, DECL_TEMPLATE_INSTANTIATIONS (tmpl));

  if (p == (struct pending_inline *)0)
    {
      /* do nothing */
    }
  else if (DECL_INLINE (fndecl))
    {
      DECL_PENDING_INLINE_INFO (fndecl) = p;
      p->next = pending_inlines;
      pending_inlines = p;
    }
  else
    {
      p->next = pending_template_expansions;
      pending_template_expansions = p;
    }
 exit:
  function_maybepermanent_obstack = old_fmp_obstack;
  pop_obstacks ();

  return fndecl;
}

/* classlevel should now never be true.  jason 4/12/94 */
void
undo_template_name_overload (id, classlevel)
     tree id;
     int classlevel;
{
  tree template;

  template = IDENTIFIER_TEMPLATE (id);
  if (!template)
    return;

#if 0 /* not yet, should get fixed properly later */
  poplevel (0, 0, 0);
#endif
#if 1 /* XXX */
  /* This was a botch... See `overload_template_name' just below.  */
  if (!classlevel)
    poplevel (0, 0, 0);
#endif
}

/* classlevel should now never be true.  jason 4/12/94 */
void
overload_template_name (id, classlevel)
     tree id;
     int classlevel;
{
  tree template, t, decl;
  struct template_info *tinfo;

  my_friendly_assert (TREE_CODE (id) == IDENTIFIER_NODE, 284);
  template = IDENTIFIER_TEMPLATE (id);
  if (!template)
    return;

  template = TREE_PURPOSE (template);
  tinfo = DECL_TEMPLATE_INFO (template);
  template = DECL_NAME (template);
  my_friendly_assert (template != NULL_TREE, 285);

#if 1 /* XXX */
  /* This was a botch... names of templates do not get their own private
     scopes.  Rather, they should go into the binding level already created
     by push_template_decls.  Except that there isn't one of those for
     specializations.  */
  if (!classlevel)
    {
      pushlevel (1);
      declare_pseudo_global_level ();
    }
#endif

  t = xref_tag (tinfo->aggr, id, NULL_TREE, 1);
  my_friendly_assert (TREE_CODE (t) == RECORD_TYPE
		      || TREE_CODE (t) == UNION_TYPE
		      || TREE_CODE (t) == UNINSTANTIATED_P_TYPE, 286);

  decl = build_decl (TYPE_DECL, template, t);
  SET_DECL_ARTIFICIAL (decl);

#if 0 /* fix this later */
  /* We don't want to call here if the work has already been done.  */
  t = (classlevel
       ? IDENTIFIER_CLASS_VALUE (template)
       : IDENTIFIER_LOCAL_VALUE (template));
  if (t
      && TREE_CODE (t) == TYPE_DECL
      && TREE_TYPE (t) == t)
    my_friendly_abort (85);
#endif

  if (classlevel)
    pushdecl_class_level (decl);
  else
    pushdecl (decl);

#if 0 /* This seems bogus to me; if it isn't, explain why.  (jason) */
  /* Fake this for now, just to make dwarfout.c happy.  It will have to
     be done in a proper way later on.  */
  DECL_CONTEXT (decl) = t;
#endif
}

extern struct pending_input *to_be_restored;

/* NAME is the IDENTIFIER value of a PRE_PARSED_CLASS_DECL. */
void
end_template_instantiation (name)
     tree name;
{
  tree t, decl;

  processing_template_defn--;
  if (!flag_external_templates)
    interface_unknown--;

  /* Restore the old parser input state.  */
  if (yychar == YYEMPTY)
    yychar = yylex ();
  if (yychar != END_OF_SAVED_INPUT)
    error ("parse error at end of class template");
  else
    {
      restore_pending_input (to_be_restored);
      to_be_restored = 0;
    }

  /* Our declarations didn't get stored in the global slot, since
     there was a (supposedly tags-transparent) scope in between.  */
  t = IDENTIFIER_TYPE_VALUE (name);
  my_friendly_assert (t != NULL_TREE
		      && TREE_CODE_CLASS (TREE_CODE (t)) == 't',
		      287);
  SET_CLASSTYPE_IMPLICIT_INSTANTIATION (t);
  /* Make methods of template classes static, unless
     -fexternal-templates is given.  */
  if (!flag_external_templates)
    SET_CLASSTYPE_INTERFACE_UNKNOWN (t);
  decl = IDENTIFIER_GLOBAL_VALUE (name);
  my_friendly_assert (TREE_CODE (decl) == TYPE_DECL, 288);

  undo_template_name_overload (name, 0);
  t = IDENTIFIER_TEMPLATE (name);
  pop_template_decls (DECL_TEMPLATE_PARMS (TREE_PURPOSE (t)), TREE_VALUE (t),
		      0);
  /* This will fix up the type-value field.  */
  pushdecl (decl);
  pop_from_top_level ();

#ifdef DWARF_DEBUGGING_INFO
  if (write_symbols == DWARF_DEBUG && TREE_CODE (decl) == TYPE_DECL)
    {
      /* We just completed the definition of a new file-scope type,
	 so we can go ahead and output debug-info for it now.  */
      TYPE_STUB_DECL (TREE_TYPE (decl)) = decl;
      rest_of_type_compilation (TREE_TYPE (decl), 1);
    }
#endif /* DWARF_DEBUGGING_INFO */

  /* Restore interface/implementation settings.	 */
  extract_interface_info ();
}

/* Store away the text of an template.  */

void
reinit_parse_for_template (yychar, d1, d2)
     int yychar;
     tree d1, d2;
{
  struct template_info *template_info;
  extern struct obstack inline_text_obstack; /* see comment in lex.c */

  if (d2 == NULL_TREE || d2 == error_mark_node)
    {
    lose:
      /* @@ Should use temp obstack, and discard results.  */
      reinit_parse_for_block (yychar, &inline_text_obstack, 1);
      return;
    }

  if (TREE_CODE (d2) == IDENTIFIER_NODE)
    d2 = IDENTIFIER_GLOBAL_VALUE (d2);
  if (!d2)
    goto lose;
  template_info = DECL_TEMPLATE_INFO (d2);
  if (!template_info)
    {
      template_info = (struct template_info *) permalloc (sizeof (struct template_info));
      bzero ((char *) template_info, sizeof (struct template_info));
      DECL_TEMPLATE_INFO (d2) = template_info;
    }
  template_info->filename = input_filename;
  template_info->lineno = lineno;
  reinit_parse_for_block (yychar, &inline_text_obstack, 1);
  template_info->text = obstack_base (&inline_text_obstack);
  template_info->length = obstack_object_size (&inline_text_obstack);
  obstack_finish (&inline_text_obstack);
  template_info->parm_vec = d1;
}

/* Type unification.

   We have a function template signature with one or more references to
   template parameters, and a parameter list we wish to fit to this
   template.  If possible, produce a list of parameters for the template
   which will cause it to fit the supplied parameter list.

   Return zero for success, 2 for an incomplete match that doesn't resolve
   all the types, and 1 for complete failure.  An error message will be
   printed only for an incomplete match.

   TPARMS[NTPARMS] is an array of template parameter types;
   TARGS[NTPARMS] is the array of template parameter values.  PARMS is
   the function template's signature (using TEMPLATE_PARM_IDX nodes),
   and ARGS is the argument list we're trying to match against it.

   If SUBR is 1, we're being called recursively (to unify the arguments of
   a function or method parameter of a function template), so don't zero
   out targs and don't fail on an incomplete match. */

int
type_unification (tparms, targs, parms, args, nsubsts, subr)
     tree tparms, *targs, parms, args;
     int *nsubsts, subr;
{
  tree parm, arg;
  int i;
  int ntparms = TREE_VEC_LENGTH (tparms);

  my_friendly_assert (TREE_CODE (tparms) == TREE_VEC, 289);
  my_friendly_assert (TREE_CODE (parms) == TREE_LIST, 290);
  /* ARGS could be NULL (via a call from parse.y to
     build_x_function_call).  */
  if (args)
    my_friendly_assert (TREE_CODE (args) == TREE_LIST, 291);
  my_friendly_assert (ntparms > 0, 292);

  if (!subr)
    bzero ((char *) targs, sizeof (tree) * ntparms);

  while (parms
	 && parms != void_list_node
	 && args
	 && args != void_list_node)
    {
      parm = TREE_VALUE (parms);
      parms = TREE_CHAIN (parms);
      arg = TREE_VALUE (args);
      args = TREE_CHAIN (args);

      if (arg == error_mark_node)
	return 1;
      if (arg == unknown_type_node)
	return 1;

      if (! uses_template_parms (parm)
	  && TREE_CODE_CLASS (TREE_CODE (arg)) != 't')
	{
	  if (can_convert_arg (parm, TREE_TYPE (arg), arg))
	    continue;
	  return 1;
	}
	
#if 0
      if (TREE_CODE (arg) == VAR_DECL)
	arg = TREE_TYPE (arg);
      else if (TREE_CODE_CLASS (TREE_CODE (arg)) == 'e')
	arg = TREE_TYPE (arg);
#else
      if (TREE_CODE_CLASS (TREE_CODE (arg)) != 't')
	{
	  my_friendly_assert (TREE_TYPE (arg) != NULL_TREE, 293);
	  if (TREE_CODE (arg) == TREE_LIST
	      && TREE_TYPE (arg) == unknown_type_node
	      && TREE_CODE (TREE_VALUE (arg)) == TEMPLATE_DECL)
	    {
	      int nsubsts, ntparms;
	      tree *targs;

	      /* Have to back unify here */
	      arg = TREE_VALUE (arg);
	      nsubsts = 0;
	      ntparms = TREE_VEC_LENGTH (DECL_TEMPLATE_PARMS (arg));
	      targs = (tree *) alloca (sizeof (tree) * ntparms);
	      parm = tree_cons (NULL_TREE, parm, NULL_TREE);
	      return type_unification (DECL_TEMPLATE_PARMS (arg), targs,
				       TYPE_ARG_TYPES (TREE_TYPE (arg)),
				       parm, &nsubsts, 0);
	    }
	  arg = TREE_TYPE (arg);
	}
#endif
      if (TREE_CODE (arg) == REFERENCE_TYPE)
	arg = TREE_TYPE (arg);

      if (TREE_CODE (parm) != REFERENCE_TYPE)
	{
	  if (TREE_CODE (arg) == FUNCTION_TYPE
	      || TREE_CODE (arg) == METHOD_TYPE)
	    arg = build_pointer_type (arg);
	  else if (TREE_CODE (arg) == ARRAY_TYPE)
	    arg = build_pointer_type (TREE_TYPE (arg));
	  else
	    arg = TYPE_MAIN_VARIANT (arg);
	}

      switch (unify (tparms, targs, ntparms, parm, arg, nsubsts))
	{
	case 0:
	  break;
	case 1:
	  return 1;
	}
    }
  /* Fail if we've reached the end of the parm list, and more args
     are present, and the parm list isn't variadic.  */
  if (args && args != void_list_node && parms == void_list_node)
    return 1;
  /* Fail if parms are left and they don't have default values.	 */
  if (parms
      && parms != void_list_node
      && TREE_PURPOSE (parms) == NULL_TREE)
    return 1;
  if (!subr)
    for (i = 0; i < ntparms; i++)
      if (!targs[i])
	{
	  error ("incomplete type unification");
	  return 2;
	}
  return 0;
}

/* Tail recursion is your friend.  */
static int
unify (tparms, targs, ntparms, parm, arg, nsubsts)
     tree tparms, *targs, parm, arg;
     int *nsubsts, ntparms;
{
  int idx;

  /* I don't think this will do the right thing with respect to types.
     But the only case I've seen it in so far has been array bounds, where
     signedness is the only information lost, and I think that will be
     okay.  */
  while (TREE_CODE (parm) == NOP_EXPR)
    parm = TREE_OPERAND (parm, 0);

  if (arg == error_mark_node)
    return 1;
  if (arg == unknown_type_node)
    return 1;
  if (arg == parm)
    return 0;

  switch (TREE_CODE (parm))
    {
    case TEMPLATE_TYPE_PARM:
      (*nsubsts)++;
      if (TEMPLATE_TYPE_TPARMLIST (parm) != tparms)
	{
	  error ("mixed template headers?!");
	  my_friendly_abort (86);
	  return 1;
	}
      idx = TEMPLATE_TYPE_IDX (parm);
#if 0
      /* Template type parameters cannot contain cv-quals; i.e.
         template <class T> void f (T& a, T& b) will not generate
	 void f (const int& a, const int& b).  */
      if (TYPE_READONLY (arg) > TYPE_READONLY (parm)
	  || TYPE_VOLATILE (arg) > TYPE_VOLATILE (parm))
	return 1;
      arg = TYPE_MAIN_VARIANT (arg);
#else
      {
	int constp = TYPE_READONLY (arg) > TYPE_READONLY (parm);
	int volatilep = TYPE_VOLATILE (arg) > TYPE_VOLATILE (parm);
	arg = cp_build_type_variant (arg, constp, volatilep);
      }
#endif
      /* Simple cases: Value already set, does match or doesn't.  */
      if (targs[idx] == arg)
	return 0;
      else if (targs[idx])
	return 1;
      /* Check for mixed types and values.  */
      if (TREE_CODE (TREE_VALUE (TREE_VEC_ELT (tparms, idx))) != TYPE_DECL)
	return 1;
      targs[idx] = arg;
      return 0;
    case TEMPLATE_CONST_PARM:
      (*nsubsts)++;
      idx = TEMPLATE_CONST_IDX (parm);
      if (targs[idx] == arg)
	return 0;
      else if (targs[idx])
	{
	  tree t = targs[idx];
	  if (TREE_CODE (t) == TREE_CODE (arg))
	    switch (TREE_CODE (arg))
	      {
	      case INTEGER_CST:
		if (tree_int_cst_equal (t, arg))
		  return 0;
		break;
	      case REAL_CST:
		if (REAL_VALUES_EQUAL (TREE_REAL_CST (t), TREE_REAL_CST (arg)))
		  return 0;
		break;
	      /* STRING_CST values are not valid template const parms.  */
	      default:
		;
	      }
	  my_friendly_abort (87);
	  return 1;
	}
/*	else if (typeof arg != tparms[idx])
	return 1;*/

      targs[idx] = copy_to_permanent (arg);
      return 0;

    case POINTER_TYPE:
      if (TREE_CODE (arg) != POINTER_TYPE)
	return 1;
      return unify (tparms, targs, ntparms, TREE_TYPE (parm), TREE_TYPE (arg),
		    nsubsts);

    case REFERENCE_TYPE:
      if (TREE_CODE (arg) == REFERENCE_TYPE)
	arg = TREE_TYPE (arg);
      return unify (tparms, targs, ntparms, TREE_TYPE (parm), arg, nsubsts);

    case ARRAY_TYPE:
      if (TREE_CODE (arg) != ARRAY_TYPE)
	return 1;
      if (unify (tparms, targs, ntparms, TYPE_DOMAIN (parm), TYPE_DOMAIN (arg),
		 nsubsts) != 0)
	return 1;
      return unify (tparms, targs, ntparms, TREE_TYPE (parm), TREE_TYPE (arg),
		    nsubsts);

    case REAL_TYPE:
    case INTEGER_TYPE:
      if (TREE_CODE (arg) != TREE_CODE (parm))
	return 1;

      if (TREE_CODE (parm) == INTEGER_TYPE)
	{
	  if (TYPE_MIN_VALUE (parm) && TYPE_MIN_VALUE (arg)
	      && unify (tparms, targs, ntparms,
			TYPE_MIN_VALUE (parm), TYPE_MIN_VALUE (arg), nsubsts))
	    return 1;
	  if (TYPE_MAX_VALUE (parm) && TYPE_MAX_VALUE (arg)
	      && unify (tparms, targs, ntparms,
			TYPE_MAX_VALUE (parm), TYPE_MAX_VALUE (arg), nsubsts))
	    return 1;
	}
      /* As far as unification is concerned, this wins.	 Later checks
	 will invalidate it if necessary.  */
      return 0;

      /* Types INTEGER_CST and MINUS_EXPR can come from array bounds.  */
    case INTEGER_CST:
      if (TREE_CODE (arg) != INTEGER_CST)
	return 1;
      return !tree_int_cst_equal (parm, arg);

    case MINUS_EXPR:
      {
	tree t1, t2;
	t1 = TREE_OPERAND (parm, 0);
	t2 = TREE_OPERAND (parm, 1);
	return unify (tparms, targs, ntparms, t1,
		      fold (build (PLUS_EXPR, integer_type_node, arg, t2)),
		      nsubsts);
      }

    case TREE_VEC:
      {
	int i;
	if (TREE_CODE (arg) != TREE_VEC)
	  return 1;
	if (TREE_VEC_LENGTH (parm) != TREE_VEC_LENGTH (arg))
	  return 1;
	for (i = TREE_VEC_LENGTH (parm) - 1; i >= 0; i--)
	  if (unify (tparms, targs, ntparms,
		     TREE_VEC_ELT (parm, i), TREE_VEC_ELT (arg, i),
		     nsubsts))
	    return 1;
	return 0;
      }

    case UNINSTANTIATED_P_TYPE:
      {
	tree a;
	/* Unification of something that is not a class fails.  */
	if (! IS_AGGR_TYPE (arg))
	  return 1;
	a = IDENTIFIER_TEMPLATE (TYPE_IDENTIFIER (arg));
	if (a && UPT_TEMPLATE (parm) == TREE_PURPOSE (a))
	  return unify (tparms, targs, ntparms, UPT_PARMS (parm),
			TREE_VALUE (a), nsubsts);
	/* FIXME: Should check base conversions here.  */
	return 1;
      }

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_FLAG (parm))
	return unify (tparms, targs, ntparms, TYPE_PTRMEMFUNC_FN_TYPE (parm),
		      arg, nsubsts);

      /* Allow trivial conversions.  */
      if (TYPE_MAIN_VARIANT (parm) != TYPE_MAIN_VARIANT (arg)
	  || TYPE_READONLY (parm) < TYPE_READONLY (arg)
	  || TYPE_VOLATILE (parm) < TYPE_VOLATILE (arg))
	return 1;
      return 0;

    case METHOD_TYPE:
      if (TREE_CODE (arg) != METHOD_TYPE)
	return 1;
      goto check_args;

    case FUNCTION_TYPE:
      if (TREE_CODE (arg) != FUNCTION_TYPE)
	return 1;
     check_args:
      if (unify (tparms, targs, ntparms, TREE_TYPE (parm),
		 TREE_TYPE (arg), nsubsts))
	return 1;
      return type_unification (tparms, targs, TYPE_ARG_TYPES (parm),
			       TYPE_ARG_TYPES (arg), nsubsts, 1);

    case OFFSET_TYPE:
      if (TREE_CODE (arg) != OFFSET_TYPE)
	return 1;
      if (unify (tparms, targs, ntparms, TYPE_OFFSET_BASETYPE (parm),
		 TYPE_OFFSET_BASETYPE (arg), nsubsts))
	return 1;
      return unify (tparms, targs, ntparms, TREE_TYPE (parm),
		    TREE_TYPE (arg), nsubsts);

    default:
      sorry ("use of `%s' in template type unification",
	     tree_code_name [(int) TREE_CODE (parm)]);
      return 1;
    }
}


#undef DEBUG

int
do_pending_expansions ()
{
  struct pending_inline *i, *new_list = 0;

  {
    tree t;
    for (t = template_classes; t; t = TREE_CHAIN (t))
      instantiate_member_templates (TREE_PURPOSE (t));
  }
  
  if (!pending_template_expansions)
    return 0;

#ifdef DEBUG
  fprintf (stderr, "\n\n\t\t IN DO_PENDING_EXPANSIONS\n\n");
#endif

  i = pending_template_expansions;
  while (i)
    {
      tree context;

      struct pending_inline *next = i->next;
      tree t = i->fndecl;

      int decision = 0;
#define DECIDE(N) do {decision=(N); goto decided;} while(0)

      my_friendly_assert (TREE_CODE (t) == FUNCTION_DECL
			  || TREE_CODE (t) == VAR_DECL, 294);
      if (TREE_ASM_WRITTEN (t))
	DECIDE (0);

      if (DECL_EXPLICIT_INSTANTIATION (t))
	DECIDE (DECL_NOT_REALLY_EXTERN (t));
      else if (! flag_implicit_templates)
	DECIDE (0);

      if (i->interface == 1)
	/* OK, it was an implicit instantiation.  */
	{
	  if (SUPPORTS_WEAK)
	    DECL_WEAK (t) = 1;
	  else
	    TREE_PUBLIC (t) = 0;
	}

      /* If it's a method, let the class type decide it.
	 @@ What if the method template is in a separate file?
	 Maybe both file contexts should be taken into account?
	 Maybe only do this if i->interface == 1 (unknown)?  */
      context = DECL_CONTEXT (t);
      if (context != NULL_TREE
	  && TREE_CODE_CLASS (TREE_CODE (context)) == 't')
	{
	  /* I'm interested in the context of this version of the function,
	     not the original virtual declaration.  */
	  context = DECL_CLASS_CONTEXT (t);

	  /* If `unknown', we might want a static copy.
	     If `implementation', we want a global one.
	     If `interface', ext ref.  */
	  if (CLASSTYPE_INTERFACE_KNOWN (context))
	    DECIDE (!CLASSTYPE_INTERFACE_ONLY (context));
#if 1 /* This doesn't get us stuff needed only by the file initializer.  */
	  DECIDE (TREE_USED (t));
#else /* This compiles too much stuff, but that's probably better in
	 most cases than never compiling the stuff we need.  */
	  DECIDE (1);
#endif
	}

      if (i->interface == 1)
	DECIDE (TREE_USED (t));
      else
	DECIDE (i->interface);

    decided:
#ifdef DEBUG
      print_node_brief (stderr, decision ? "yes: " : "no: ", t, 0);
      fprintf (stderr, "\t%s\n",
	       (DECL_ASSEMBLER_NAME (t)
		? IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t))
		: ""));
#endif
      if (decision)
	{
	  i->next = pending_inlines;
	  pending_inlines = i;
	}
      else
	{
	  i->next = new_list;
	  new_list = i;
	}
      i = next;
    }
  pending_template_expansions = new_list;
  if (!pending_inlines)
    return 0;
  do_pending_inlines ();
  return 1;
}


struct pending_template {
  struct pending_template *next;
  tree id;
};

static struct pending_template* pending_templates;

void
do_pending_templates ()
{
  struct pending_template* t;
  
  for ( t = pending_templates; t; t = t->next)
    {
      instantiate_class_template (t->id, 1);
    }

  for ( t = pending_templates; t; t = pending_templates)
    {
      pending_templates = t->next;
      free(t);
    }
}

static void
add_pending_template (pt)
     tree pt;
{
  struct pending_template *p;
  
  p = (struct pending_template *) malloc (sizeof (struct pending_template));
  p->next = pending_templates;
  pending_templates = p;
  p->id = pt;
}

void
mark_function_instantiated (result, extern_p)
     tree result;
     int extern_p;
{
  if (DECL_TEMPLATE_INSTANTIATION (result))
    SET_DECL_EXPLICIT_INSTANTIATION (result);
  TREE_PUBLIC (result) = 1;

  if (! extern_p)
    {
      DECL_INTERFACE_KNOWN (result) = 1;
      DECL_NOT_REALLY_EXTERN (result) = 1;
    }
}

/* called from the parser.  */
void
do_function_instantiation (declspecs, declarator, storage)
     tree declspecs, declarator, storage;
{
  tree decl = grokdeclarator (declarator, declspecs, NORMAL, 0,
			      NULL_TREE, NULL_TREE);
  tree name;
  tree fn;
  tree result = NULL_TREE;
  int extern_p = 0;

  /* If we've already seen this template instance, use it.  */
  if (name = DECL_ASSEMBLER_NAME (decl),
      fn = IDENTIFIER_GLOBAL_VALUE (name),
      fn && DECL_TEMPLATE_INSTANTIATION (fn))
    result = fn;
  else if (name = DECL_NAME (decl), fn = IDENTIFIER_GLOBAL_VALUE (name), fn)
    {
      for (fn = get_first_fn (fn); fn; fn = DECL_CHAIN (fn))
	if (decls_match (fn, decl)
	    && DECL_DEFER_OUTPUT (fn))
	  {
	    result = fn;
	    break;
	  }
	else if (TREE_CODE (fn) == TEMPLATE_DECL)
	  {
	    int ntparms = TREE_VEC_LENGTH (DECL_TEMPLATE_PARMS (fn));
	    tree *targs = (tree *) malloc (sizeof (tree) * ntparms);
	    int i, dummy = 0;
	    i = type_unification (DECL_TEMPLATE_PARMS (fn), targs,
				  TYPE_ARG_TYPES (TREE_TYPE (fn)),
				  TYPE_ARG_TYPES (TREE_TYPE (decl)),
				  &dummy, 0);
	    if (i == 0)
	      {
		if (result)
		  cp_error ("ambiguous template instantiation for `%D' requested", decl);
		else
		  result = instantiate_template (fn, targs);
	      }
	    free (targs);
	  }
    }
  if (! result)
    {
      cp_error ("no matching template for `%D' found", decl);
      return;
    }

  if (flag_external_templates)
    return;

  if (storage == NULL_TREE)
    ;
  else if (storage == ridpointers[(int) RID_EXTERN])
    extern_p = 1;
  else
    cp_error ("storage class `%D' applied to template instantiation",
	      storage);
  mark_function_instantiated (result, extern_p);
  repo_template_instantiated (result, extern_p);
}

void
mark_class_instantiated (t, extern_p)
     tree t;
     int extern_p;
{
  SET_CLASSTYPE_EXPLICIT_INSTANTIATION (t);
  SET_CLASSTYPE_INTERFACE_KNOWN (t);
  CLASSTYPE_INTERFACE_ONLY (t) = extern_p;
  CLASSTYPE_VTABLE_NEEDS_WRITING (t) = ! extern_p;
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (t)) = extern_p;
  if (! extern_p)
    {
      CLASSTYPE_DEBUG_REQUESTED (t) = 1;
      rest_of_type_compilation (t, 1);
    }
}     

void
do_type_instantiation (name, storage)
     tree name, storage;
{
  tree t = TREE_TYPE (name);
  int extern_p = 0;
  int nomem_p = 0;

  /* With -fexternal-templates, explicit instantiations are treated the same
     as implicit ones.  */
  if (flag_external_templates)
    return;

  if (TYPE_SIZE (t) == NULL_TREE)
    {
      cp_error ("explicit instantiation of `%#T' before definition of template",
		t);
      return;
    }

  if (storage == NULL_TREE)
    /* OK */;
  else if (storage == ridpointers[(int) RID_INLINE])
    nomem_p = 1;
  else if (storage == ridpointers[(int) RID_EXTERN])
    extern_p = 1;
  else
    {
      cp_error ("storage class `%D' applied to template instantiation",
		storage);
      extern_p = 0;
    }

  /* We've already instantiated this.  */
  if (CLASSTYPE_EXPLICIT_INSTANTIATION (t) && ! CLASSTYPE_INTERFACE_ONLY (t)
      && extern_p)
    return;

  if (! CLASSTYPE_TEMPLATE_SPECIALIZATION (t))
    {
      mark_class_instantiated (t, extern_p);
      repo_template_instantiated (t, extern_p);
    }

  if (nomem_p)
    return;

  {
    tree tmp;
    /* Classes nested in template classes currently don't have an
       IDENTIFIER_TEMPLATE--their out-of-line members are handled
       by the enclosing template class.  Note that there are name
       conflict bugs with this approach. */
    tmp = TYPE_IDENTIFIER (t);
    if (IDENTIFIER_TEMPLATE (tmp))
      instantiate_member_templates (tmp);

    /* this should really be done by instantiate_member_templates */
    tmp = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (t), 0);
    for (; tmp; tmp = TREE_CHAIN (tmp))
      if (DECL_TEMPLATE_INSTANTIATION (tmp))
	{
	  mark_function_instantiated (tmp, extern_p);
	  repo_template_instantiated (tmp, extern_p);
	}

#if 0
    for (tmp = TYPE_FIELDS (t); tmp; tmp = TREE_CHAIN (tmp))
      {
	if (TREE_CODE (tmp) == VAR_DECL)
	  /* eventually do something */;
      }
#endif

    for (tmp = CLASSTYPE_TAGS (t); tmp; tmp = TREE_CHAIN (tmp))
      if (IS_AGGR_TYPE (TREE_VALUE (tmp)))
	do_type_instantiation (TYPE_MAIN_DECL (TREE_VALUE (tmp)), storage);
  }
}

tree
create_nested_upt (scope, name)
     tree scope, name;
{
  tree t = make_lang_type (UNINSTANTIATED_P_TYPE);
  tree d = build_decl (TYPE_DECL, name, t);

  TYPE_NAME (t) = d;
  TYPE_VALUES (t) = TYPE_VALUES (scope);
  TYPE_CONTEXT (t) = scope;

  pushdecl (d);
  return d;
}
