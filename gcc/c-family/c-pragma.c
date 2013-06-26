/* Handle #pragma, system V.4 style.  Supports #pragma weak and #pragma pack.
   Copyright (C) 1992-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "function.h"		/* For cfun.  FIXME: Does the parser know
				   when it is inside a function, so that
				   we don't have to look at cfun?  */
#include "cpplib.h"
#include "c-pragma.h"
#include "flags.h"
#include "c-common.h"
#include "tm_p.h"		/* For REGISTER_TARGET_PRAGMAS (why is
				   this not a target hook?).  */
#include "vec.h"
#include "target.h"
#include "diagnostic.h"
#include "opts.h"
#include "plugin.h"
#include "cgraph.h"

#define GCC_BAD(gmsgid) \
  do { warning (OPT_Wpragmas, gmsgid); return; } while (0)
#define GCC_BAD2(gmsgid, arg) \
  do { warning (OPT_Wpragmas, gmsgid, arg); return; } while (0)

typedef struct GTY(()) align_stack {
  int		       alignment;
  tree		       id;
  struct align_stack * prev;
} align_stack;

static GTY(()) struct align_stack * alignment_stack;

static void handle_pragma_pack (cpp_reader *);

/* If we have a "global" #pragma pack(<n>) in effect when the first
   #pragma pack(push,<n>) is encountered, this stores the value of
   maximum_field_alignment in effect.  When the final pop_alignment()
   happens, we restore the value to this, not to a value of 0 for
   maximum_field_alignment.  Value is in bits.  */
static int default_alignment;
#define SET_GLOBAL_ALIGNMENT(ALIGN) (maximum_field_alignment = *(alignment_stack == NULL \
	? &default_alignment \
	: &alignment_stack->alignment) = (ALIGN))

static void push_alignment (int, tree);
static void pop_alignment (tree);

/* Push an alignment value onto the stack.  */
static void
push_alignment (int alignment, tree id)
{
  align_stack * entry;

  entry = ggc_alloc_align_stack ();

  entry->alignment  = alignment;
  entry->id	    = id;
  entry->prev	    = alignment_stack;

  /* The current value of maximum_field_alignment is not necessarily
     0 since there may be a #pragma pack(<n>) in effect; remember it
     so that we can restore it after the final #pragma pop().  */
  if (alignment_stack == NULL)
    default_alignment = maximum_field_alignment;

  alignment_stack = entry;

  maximum_field_alignment = alignment;
}

/* Undo a push of an alignment onto the stack.  */
static void
pop_alignment (tree id)
{
  align_stack * entry;

  if (alignment_stack == NULL)
    GCC_BAD ("#pragma pack (pop) encountered without matching #pragma pack (push)");

  /* If we got an identifier, strip away everything above the target
     entry so that the next step will restore the state just below it.  */
  if (id)
    {
      for (entry = alignment_stack; entry; entry = entry->prev)
	if (entry->id == id)
	  {
	    alignment_stack = entry;
	    break;
	  }
      if (entry == NULL)
	warning (OPT_Wpragmas, "\
#pragma pack(pop, %E) encountered without matching #pragma pack(push, %E)"
		 , id, id);
    }

  entry = alignment_stack->prev;

  maximum_field_alignment = entry ? entry->alignment : default_alignment;

  alignment_stack = entry;
}

/* #pragma pack ()
   #pragma pack (N)

   #pragma pack (push)
   #pragma pack (push, N)
   #pragma pack (push, ID)
   #pragma pack (push, ID, N)
   #pragma pack (pop)
   #pragma pack (pop, ID) */
static void
handle_pragma_pack (cpp_reader * ARG_UNUSED (dummy))
{
  tree x, id = 0;
  int align = -1;
  enum cpp_ttype token;
  enum { set, push, pop } action;

  if (pragma_lex (&x) != CPP_OPEN_PAREN)
    GCC_BAD ("missing %<(%> after %<#pragma pack%> - ignored");

  token = pragma_lex (&x);
  if (token == CPP_CLOSE_PAREN)
    {
      action = set;
      align = initial_max_fld_align;
    }
  else if (token == CPP_NUMBER)
    {
      if (TREE_CODE (x) != INTEGER_CST)
	GCC_BAD ("invalid constant in %<#pragma pack%> - ignored");
      align = TREE_INT_CST_LOW (x);
      action = set;
      if (pragma_lex (&x) != CPP_CLOSE_PAREN)
	GCC_BAD ("malformed %<#pragma pack%> - ignored");
    }
  else if (token == CPP_NAME)
    {
#define GCC_BAD_ACTION do { if (action != pop) \
	  GCC_BAD ("malformed %<#pragma pack(push[, id][, <n>])%> - ignored"); \
	else \
	  GCC_BAD ("malformed %<#pragma pack(pop[, id])%> - ignored"); \
	} while (0)

      const char *op = IDENTIFIER_POINTER (x);
      if (!strcmp (op, "push"))
	action = push;
      else if (!strcmp (op, "pop"))
	action = pop;
      else
	GCC_BAD2 ("unknown action %qE for %<#pragma pack%> - ignored", x);

      while ((token = pragma_lex (&x)) == CPP_COMMA)
	{
	  token = pragma_lex (&x);
	  if (token == CPP_NAME && id == 0)
	    {
	      id = x;
	    }
	  else if (token == CPP_NUMBER && action == push && align == -1)
	    {
	      if (TREE_CODE (x) != INTEGER_CST)
		GCC_BAD ("invalid constant in %<#pragma pack%> - ignored");
	      align = TREE_INT_CST_LOW (x);
	      if (align == -1)
		action = set;
	    }
	  else
	    GCC_BAD_ACTION;
	}

      if (token != CPP_CLOSE_PAREN)
	GCC_BAD_ACTION;
#undef GCC_BAD_ACTION
    }
  else
    GCC_BAD ("malformed %<#pragma pack%> - ignored");

  if (pragma_lex (&x) != CPP_EOF)
    warning (OPT_Wpragmas, "junk at end of %<#pragma pack%>");

  if (flag_pack_struct)
    GCC_BAD ("#pragma pack has no effect with -fpack-struct - ignored");

  if (action != pop)
    switch (align)
      {
      case 0:
      case 1:
      case 2:
      case 4:
      case 8:
      case 16:
	align *= BITS_PER_UNIT;
	break;
      case -1:
	if (action == push)
	  {
	    align = maximum_field_alignment;
	    break;
	  }
      default:
	GCC_BAD2 ("alignment must be a small power of two, not %d", align);
      }

  switch (action)
    {
    case set:   SET_GLOBAL_ALIGNMENT (align);  break;
    case push:  push_alignment (align, id);    break;
    case pop:   pop_alignment (id);	       break;
    }
}

typedef struct GTY(()) pending_weak_d
{
  tree name;
  tree value;
} pending_weak;


static GTY(()) vec<pending_weak, va_gc> *pending_weaks;

static void apply_pragma_weak (tree, tree);
static void handle_pragma_weak (cpp_reader *);

static void
apply_pragma_weak (tree decl, tree value)
{
  if (value)
    {
      value = build_string (IDENTIFIER_LENGTH (value),
			    IDENTIFIER_POINTER (value));
      decl_attributes (&decl, build_tree_list (get_identifier ("alias"),
					       build_tree_list (NULL, value)),
		       0);
    }

  if (SUPPORTS_WEAK && DECL_EXTERNAL (decl) && TREE_USED (decl)
      && !DECL_WEAK (decl) /* Don't complain about a redundant #pragma.  */
      && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
    warning (OPT_Wpragmas, "applying #pragma weak %q+D after first use "
	     "results in unspecified behavior", decl);

  declare_weak (decl);
}

void
maybe_apply_pragma_weak (tree decl)
{
  tree id;
  int i;
  pending_weak *pe;

  /* Avoid asking for DECL_ASSEMBLER_NAME when it's not needed.  */

  /* No weak symbols pending, take the short-cut.  */
  if (!pending_weaks)
    return;
  /* If it's not visible outside this file, it doesn't matter whether
     it's weak.  */
  if (!DECL_EXTERNAL (decl) && !TREE_PUBLIC (decl))
    return;
  /* If it's not a function or a variable, it can't be weak.
     FIXME: what kinds of things are visible outside this file but
     aren't functions or variables?   Should this be an assert instead?  */
  if (TREE_CODE (decl) != FUNCTION_DECL && TREE_CODE (decl) != VAR_DECL)
    return;

  id = DECL_ASSEMBLER_NAME (decl);

  FOR_EACH_VEC_ELT (*pending_weaks, i, pe)
    if (id == pe->name)
      {
	apply_pragma_weak (decl, pe->value);
	pending_weaks->unordered_remove (i);
	break;
      }
}

/* Process all "#pragma weak A = B" directives where we have not seen
   a decl for A.  */
void
maybe_apply_pending_pragma_weaks (void)
{
  tree alias_id, id, decl;
  int i;
  pending_weak *pe;
  symtab_node target;

  if (!pending_weaks)
    return;

  FOR_EACH_VEC_ELT (*pending_weaks, i, pe)
    {
      alias_id = pe->name;
      id = pe->value;

      if (id == NULL)
	continue;

      target = symtab_node_for_asm (id);
      decl = build_decl (UNKNOWN_LOCATION,
			 target ? TREE_CODE (target->symbol.decl) : FUNCTION_DECL,
			 alias_id, default_function_type);

      DECL_ARTIFICIAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
      DECL_WEAK (decl) = 1;
      if (TREE_CODE (decl) == VAR_DECL)
	TREE_STATIC (decl) = 1;
      if (!target)
	{
	  error ("%q+D aliased to undefined symbol %qE",
		 decl, id);
	  continue;
	}

      assemble_alias (decl, id);
    }
}

/* #pragma weak name [= value] */
static void
handle_pragma_weak (cpp_reader * ARG_UNUSED (dummy))
{
  tree name, value, x, decl;
  enum cpp_ttype t;

  value = 0;

  if (pragma_lex (&name) != CPP_NAME)
    GCC_BAD ("malformed #pragma weak, ignored");
  t = pragma_lex (&x);
  if (t == CPP_EQ)
    {
      if (pragma_lex (&value) != CPP_NAME)
	GCC_BAD ("malformed #pragma weak, ignored");
      t = pragma_lex (&x);
    }
  if (t != CPP_EOF)
    warning (OPT_Wpragmas, "junk at end of %<#pragma weak%>");

  decl = identifier_global_value (name);
  if (decl && DECL_P (decl))
    {
      apply_pragma_weak (decl, value);
      if (value)
	{
	  DECL_EXTERNAL (decl) = 0;
	  if (TREE_CODE (decl) == VAR_DECL)
	    TREE_STATIC (decl) = 1;
	  assemble_alias (decl, value);
	}
    }
  else
    {
      pending_weak pe = {name, value};
      vec_safe_push (pending_weaks, pe);
    }
}

/* GCC supports two #pragma directives for renaming the external
   symbol associated with a declaration (DECL_ASSEMBLER_NAME), for
   compatibility with the Solaris and VMS system headers.  GCC also
   has its own notation for this, __asm__("name") annotations.

   Corner cases of these features and their interaction:

   1) Both pragmas silently apply only to declarations with external
      linkage (that is, TREE_PUBLIC || DECL_EXTERNAL).  Asm labels
      do not have this restriction.

   2) In C++, both #pragmas silently apply only to extern "C" declarations.
      Asm labels do not have this restriction.

   3) If any of the three ways of changing DECL_ASSEMBLER_NAME is
      applied to a decl whose DECL_ASSEMBLER_NAME is already set, and the
      new name is different, a warning issues and the name does not change.

   4) The "source name" for #pragma redefine_extname is the DECL_NAME,
      *not* the DECL_ASSEMBLER_NAME.

   5) If #pragma extern_prefix is in effect and a declaration occurs
      with an __asm__ name, the #pragma extern_prefix is silently
      ignored for that declaration.

   6) If #pragma extern_prefix and #pragma redefine_extname apply to
      the same declaration, whichever triggered first wins, and a warning
      is issued.  (We would like to have #pragma redefine_extname always
      win, but it can appear either before or after the declaration, and
      if it appears afterward, we have no way of knowing whether a modified
      DECL_ASSEMBLER_NAME is due to #pragma extern_prefix.)  */

typedef struct GTY(()) pending_redefinition_d {
  tree oldname;
  tree newname;
} pending_redefinition;


static GTY(()) vec<pending_redefinition, va_gc> *pending_redefine_extname;

static void handle_pragma_redefine_extname (cpp_reader *);

/* #pragma redefine_extname oldname newname */
static void
handle_pragma_redefine_extname (cpp_reader * ARG_UNUSED (dummy))
{
  tree oldname, newname, decls, x;
  enum cpp_ttype t;
  bool found;

  if (pragma_lex (&oldname) != CPP_NAME)
    GCC_BAD ("malformed #pragma redefine_extname, ignored");
  if (pragma_lex (&newname) != CPP_NAME)
    GCC_BAD ("malformed #pragma redefine_extname, ignored");
  t = pragma_lex (&x);
  if (t != CPP_EOF)
    warning (OPT_Wpragmas, "junk at end of %<#pragma redefine_extname%>");

  found = false;
  for (decls = c_linkage_bindings (oldname);
       decls; )
    {
      tree decl;
      if (TREE_CODE (decls) == TREE_LIST)
	{
	  decl = TREE_VALUE (decls);
	  decls = TREE_CHAIN (decls);
	}
      else
	{
	  decl = decls;
	  decls = NULL_TREE;
	}

      if ((TREE_PUBLIC (decl) || DECL_EXTERNAL (decl))
	  && (TREE_CODE (decl) == FUNCTION_DECL
	      || TREE_CODE (decl) == VAR_DECL))
	{
	  found = true;
	  if (DECL_ASSEMBLER_NAME_SET_P (decl))
	    {
	      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
	      name = targetm.strip_name_encoding (name);

	      if (strcmp (name, IDENTIFIER_POINTER (newname)))
		warning (OPT_Wpragmas, "#pragma redefine_extname ignored due to "
			 "conflict with previous rename");
	    }
	  else
	    change_decl_assembler_name (decl, newname);
	}
    }

  if (!found)
    /* We have to add this to the rename list even if there's already
       a global value that doesn't meet the above criteria, because in
       C++ "struct foo {...};" puts "foo" in the current namespace but
       does *not* conflict with a subsequent declaration of a function
       or variable foo.  See g++.dg/other/pragma-re-2.C.  */
    add_to_renaming_pragma_list (oldname, newname);
}

/* This is called from here and from ia64-c.c.  */
void
add_to_renaming_pragma_list (tree oldname, tree newname)
{
  unsigned ix;
  pending_redefinition *p;

  FOR_EACH_VEC_SAFE_ELT (pending_redefine_extname, ix, p)
    if (oldname == p->oldname)
      {
	if (p->newname != newname)
	  warning (OPT_Wpragmas, "#pragma redefine_extname ignored due to "
		   "conflict with previous #pragma redefine_extname");
	return;
      }

  pending_redefinition e = {oldname, newname};
  vec_safe_push (pending_redefine_extname, e);
}

/* The current prefix set by #pragma extern_prefix.  */
GTY(()) tree pragma_extern_prefix;

/* Hook from the front ends to apply the results of one of the preceding
   pragmas that rename variables.  */

tree
maybe_apply_renaming_pragma (tree decl, tree asmname)
{
  unsigned ix;
  pending_redefinition *p;

  /* The renaming pragmas are only applied to declarations with
     external linkage.  */
  if ((TREE_CODE (decl) != FUNCTION_DECL && TREE_CODE (decl) != VAR_DECL)
      || (!TREE_PUBLIC (decl) && !DECL_EXTERNAL (decl))
      || !has_c_linkage (decl))
    return asmname;

  /* If the DECL_ASSEMBLER_NAME is already set, it does not change,
     but we may warn about a rename that conflicts.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl))
    {
      const char *oldname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      oldname = targetm.strip_name_encoding (oldname);

      if (asmname && strcmp (TREE_STRING_POINTER (asmname), oldname))
	  warning (OPT_Wpragmas, "asm declaration ignored due to "
		   "conflict with previous rename");

      /* Take any pending redefine_extname off the list.  */
      FOR_EACH_VEC_SAFE_ELT (pending_redefine_extname, ix, p)
	if (DECL_NAME (decl) == p->oldname)
	  {
	    /* Only warn if there is a conflict.  */
	    if (strcmp (IDENTIFIER_POINTER (p->newname), oldname))
	      warning (OPT_Wpragmas, "#pragma redefine_extname ignored due to "
		       "conflict with previous rename");

	    pending_redefine_extname->unordered_remove (ix);
	    break;
	  }
      return 0;
    }

  /* Find out if we have a pending #pragma redefine_extname.  */
  FOR_EACH_VEC_SAFE_ELT (pending_redefine_extname, ix, p)
    if (DECL_NAME (decl) == p->oldname)
      {
	tree newname = p->newname;
	pending_redefine_extname->unordered_remove (ix);

	/* If we already have an asmname, #pragma redefine_extname is
	   ignored (with a warning if it conflicts).  */
	if (asmname)
	  {
	    if (strcmp (TREE_STRING_POINTER (asmname),
			IDENTIFIER_POINTER (newname)) != 0)
	      warning (OPT_Wpragmas, "#pragma redefine_extname ignored due to "
		       "conflict with __asm__ declaration");
	    return asmname;
	  }

	/* Otherwise we use what we've got; #pragma extern_prefix is
	   silently ignored.  */
	return build_string (IDENTIFIER_LENGTH (newname),
			     IDENTIFIER_POINTER (newname));
      }

  /* If we've got an asmname, #pragma extern_prefix is silently ignored.  */
  if (asmname)
    return asmname;

  /* If #pragma extern_prefix is in effect, apply it.  */
  if (pragma_extern_prefix)
    {
      const char *prefix = TREE_STRING_POINTER (pragma_extern_prefix);
      size_t plen = TREE_STRING_LENGTH (pragma_extern_prefix) - 1;

      const char *id = IDENTIFIER_POINTER (DECL_NAME (decl));
      size_t ilen = IDENTIFIER_LENGTH (DECL_NAME (decl));

      char *newname = (char *) alloca (plen + ilen + 1);

      memcpy (newname,        prefix, plen);
      memcpy (newname + plen, id, ilen + 1);

      return build_string (plen + ilen, newname);
    }

  /* Nada.  */
  return 0;
}


static void handle_pragma_visibility (cpp_reader *);

static vec<int> visstack;

/* Push the visibility indicated by STR onto the top of the #pragma
   visibility stack.  KIND is 0 for #pragma GCC visibility, 1 for
   C++ namespace with visibility attribute and 2 for C++ builtin
   ABI namespace.  push_visibility/pop_visibility calls must have
   matching KIND, it is not allowed to push visibility using one
   KIND and pop using a different one.  */

void
push_visibility (const char *str, int kind)
{
  visstack.safe_push (((int) default_visibility) | (kind << 8));
  if (!strcmp (str, "default"))
    default_visibility = VISIBILITY_DEFAULT;
  else if (!strcmp (str, "internal"))
    default_visibility = VISIBILITY_INTERNAL;
  else if (!strcmp (str, "hidden"))
    default_visibility = VISIBILITY_HIDDEN;
  else if (!strcmp (str, "protected"))
    default_visibility = VISIBILITY_PROTECTED;
  else
    GCC_BAD ("#pragma GCC visibility push() must specify default, internal, hidden or protected");
  visibility_options.inpragma = 1;
}

/* Pop a level of the #pragma visibility stack.  Return true if
   successful.  */

bool
pop_visibility (int kind)
{
  if (!visstack.length ())
    return false;
  if ((visstack.last () >> 8) != kind)
    return false;
  default_visibility
    = (enum symbol_visibility) (visstack.pop () & 0xff);
  visibility_options.inpragma
    = visstack.length () != 0;
  return true;
}

/* Sets the default visibility for symbols to something other than that
   specified on the command line.  */

static void
handle_pragma_visibility (cpp_reader *dummy ATTRIBUTE_UNUSED)
{
  /* Form is #pragma GCC visibility push(hidden)|pop */
  tree x;
  enum cpp_ttype token;
  enum { bad, push, pop } action = bad;

  token = pragma_lex (&x);
  if (token == CPP_NAME)
    {
      const char *op = IDENTIFIER_POINTER (x);
      if (!strcmp (op, "push"))
	action = push;
      else if (!strcmp (op, "pop"))
	action = pop;
    }
  if (bad == action)
    GCC_BAD ("#pragma GCC visibility must be followed by push or pop");
  else
    {
      if (pop == action)
	{
	  if (! pop_visibility (0))
	    GCC_BAD ("no matching push for %<#pragma GCC visibility pop%>");
	}
      else
	{
	  if (pragma_lex (&x) != CPP_OPEN_PAREN)
	    GCC_BAD ("missing %<(%> after %<#pragma GCC visibility push%> - ignored");
	  token = pragma_lex (&x);
	  if (token != CPP_NAME)
	    GCC_BAD ("malformed #pragma GCC visibility push");
	  else
	    push_visibility (IDENTIFIER_POINTER (x), 0);
	  if (pragma_lex (&x) != CPP_CLOSE_PAREN)
	    GCC_BAD ("missing %<(%> after %<#pragma GCC visibility push%> - ignored");
	}
    }
  if (pragma_lex (&x) != CPP_EOF)
    warning (OPT_Wpragmas, "junk at end of %<#pragma GCC visibility%>");
}

static void
handle_pragma_diagnostic(cpp_reader *ARG_UNUSED(dummy))
{
  const char *kind_string, *option_string;
  unsigned int option_index;
  enum cpp_ttype token;
  diagnostic_t kind;
  tree x;
  struct cl_option_handlers handlers;

  token = pragma_lex (&x);
  if (token != CPP_NAME)
    GCC_BAD ("missing [error|warning|ignored] after %<#pragma GCC diagnostic%>");
  kind_string = IDENTIFIER_POINTER (x);
  if (strcmp (kind_string, "error") == 0)
    kind = DK_ERROR;
  else if (strcmp (kind_string, "warning") == 0)
    kind = DK_WARNING;
  else if (strcmp (kind_string, "ignored") == 0)
    kind = DK_IGNORED;
  else if (strcmp (kind_string, "push") == 0)
    {
      diagnostic_push_diagnostics (global_dc, input_location);
      return;
    }
  else if (strcmp (kind_string, "pop") == 0)
    {
      diagnostic_pop_diagnostics (global_dc, input_location);
      return;
    }
  else
    GCC_BAD ("expected [error|warning|ignored|push|pop] after %<#pragma GCC diagnostic%>");

  token = pragma_lex (&x);
  if (token != CPP_STRING)
    GCC_BAD ("missing option after %<#pragma GCC diagnostic%> kind");
  option_string = TREE_STRING_POINTER (x);
  set_default_handlers (&handlers);
  for (option_index = 0; option_index < cl_options_count; option_index++)
    if (strcmp (cl_options[option_index].opt_text, option_string) == 0)
      {
	control_warning_option (option_index, (int) kind, kind != DK_IGNORED,
				input_location, c_family_lang_mask, &handlers,
				&global_options, &global_options_set,
				global_dc);
	return;
      }
  GCC_BAD ("unknown option after %<#pragma GCC diagnostic%> kind");
}

/*  Parse #pragma GCC target (xxx) to set target specific options.  */
static void
handle_pragma_target(cpp_reader *ARG_UNUSED(dummy))
{
  enum cpp_ttype token;
  tree x;
  bool close_paren_needed_p = false;

  if (cfun)
    {
      error ("#pragma GCC option is not allowed inside functions");
      return;
    }

  token = pragma_lex (&x);
  if (token == CPP_OPEN_PAREN)
    {
      close_paren_needed_p = true;
      token = pragma_lex (&x);
    }

  if (token != CPP_STRING)
    {
      GCC_BAD ("%<#pragma GCC option%> is not a string");
      return;
    }

  /* Strings are user options.  */
  else
    {
      tree args = NULL_TREE;

      do
	{
	  /* Build up the strings now as a tree linked list.  Skip empty
	     strings.  */
	  if (TREE_STRING_LENGTH (x) > 0)
	    args = tree_cons (NULL_TREE, x, args);

	  token = pragma_lex (&x);
	  while (token == CPP_COMMA)
	    token = pragma_lex (&x);
	}
      while (token == CPP_STRING);

      if (close_paren_needed_p)
	{
	  if (token == CPP_CLOSE_PAREN)
	    token = pragma_lex (&x);
	  else
	    GCC_BAD ("%<#pragma GCC target (string [,string]...)%> does "
		     "not have a final %<)%>");
	}

      if (token != CPP_EOF)
	{
	  error ("#pragma GCC target string... is badly formed");
	  return;
	}

      /* put arguments in the order the user typed them.  */
      args = nreverse (args);

      if (targetm.target_option.pragma_parse (args, NULL_TREE))
	current_target_pragma = args;
    }
}

/* Handle #pragma GCC optimize to set optimization options.  */
static void
handle_pragma_optimize (cpp_reader *ARG_UNUSED(dummy))
{
  enum cpp_ttype token;
  tree x;
  bool close_paren_needed_p = false;
  tree optimization_previous_node = optimization_current_node;

  if (cfun)
    {
      error ("#pragma GCC optimize is not allowed inside functions");
      return;
    }

  token = pragma_lex (&x);
  if (token == CPP_OPEN_PAREN)
    {
      close_paren_needed_p = true;
      token = pragma_lex (&x);
    }

  if (token != CPP_STRING && token != CPP_NUMBER)
    {
      GCC_BAD ("%<#pragma GCC optimize%> is not a string or number");
      return;
    }

  /* Strings/numbers are user options.  */
  else
    {
      tree args = NULL_TREE;

      do
	{
	  /* Build up the numbers/strings now as a list.  */
	  if (token != CPP_STRING || TREE_STRING_LENGTH (x) > 0)
	    args = tree_cons (NULL_TREE, x, args);

	  token = pragma_lex (&x);
	  while (token == CPP_COMMA)
	    token = pragma_lex (&x);
	}
      while (token == CPP_STRING || token == CPP_NUMBER);

      if (close_paren_needed_p)
	{
	  if (token == CPP_CLOSE_PAREN)
	    token = pragma_lex (&x);
	  else
	    GCC_BAD ("%<#pragma GCC optimize (string [,string]...)%> does "
		     "not have a final %<)%>");
	}

      if (token != CPP_EOF)
	{
	  error ("#pragma GCC optimize string... is badly formed");
	  return;
	}

      /* put arguments in the order the user typed them.  */
      args = nreverse (args);

      parse_optimize_options (args, false);
      current_optimize_pragma = chainon (current_optimize_pragma, args);
      optimization_current_node = build_optimization_node ();
      c_cpp_builtins_optimize_pragma (parse_in,
				      optimization_previous_node,
				      optimization_current_node);
    }
}

/* Stack of the #pragma GCC options created with #pragma GCC push_option.  Save
   both the binary representation of the options and the TREE_LIST of
   strings that will be added to the function's attribute list.  */
typedef struct GTY(()) opt_stack {
  struct opt_stack *prev;
  tree target_binary;
  tree target_strings;
  tree optimize_binary;
  tree optimize_strings;
} opt_stack;

static GTY(()) struct opt_stack * options_stack;

/* Handle #pragma GCC push_options to save the current target and optimization
   options.  */

static void
handle_pragma_push_options (cpp_reader *ARG_UNUSED(dummy))
{
  enum cpp_ttype token;
  tree x = 0;
  opt_stack *p;

  token = pragma_lex (&x);
  if (token != CPP_EOF)
    {
      warning (OPT_Wpragmas, "junk at end of %<#pragma push_options%>");
      return;
    }

  p = ggc_alloc_opt_stack ();
  p->prev = options_stack;
  options_stack = p;

  /* Save optimization and target flags in binary format.  */
  p->optimize_binary = build_optimization_node ();
  p->target_binary = build_target_option_node ();

  /* Save optimization and target flags in string list format.  */
  p->optimize_strings = copy_list (current_optimize_pragma);
  p->target_strings = copy_list (current_target_pragma);
}

/* Handle #pragma GCC pop_options to restore the current target and
   optimization options from a previous push_options.  */

static void
handle_pragma_pop_options (cpp_reader *ARG_UNUSED(dummy))
{
  enum cpp_ttype token;
  tree x = 0;
  opt_stack *p;

  token = pragma_lex (&x);
  if (token != CPP_EOF)
    {
      warning (OPT_Wpragmas, "junk at end of %<#pragma pop_options%>");
      return;
    }

  if (! options_stack)
    {
      warning (OPT_Wpragmas,
	       "%<#pragma GCC pop_options%> without a corresponding "
	       "%<#pragma GCC push_options%>");
      return;
    }

  p = options_stack;
  options_stack = p->prev;

  if (p->target_binary != target_option_current_node)
    {
      (void) targetm.target_option.pragma_parse (NULL_TREE, p->target_binary);
      target_option_current_node = p->target_binary;
    }

  if (p->optimize_binary != optimization_current_node)
    {
      tree old_optimize = optimization_current_node;
      cl_optimization_restore (&global_options,
			       TREE_OPTIMIZATION (p->optimize_binary));
      c_cpp_builtins_optimize_pragma (parse_in, old_optimize,
				      p->optimize_binary);
      optimization_current_node = p->optimize_binary;
    }

  current_target_pragma = p->target_strings;
  current_optimize_pragma = p->optimize_strings;
}

/* Handle #pragma GCC reset_options to restore the current target and
   optimization options to the original options used on the command line.  */

static void
handle_pragma_reset_options (cpp_reader *ARG_UNUSED(dummy))
{
  enum cpp_ttype token;
  tree x = 0;
  tree new_optimize = optimization_default_node;
  tree new_target = target_option_default_node;

  token = pragma_lex (&x);
  if (token != CPP_EOF)
    {
      warning (OPT_Wpragmas, "junk at end of %<#pragma reset_options%>");
      return;
    }

  if (new_target != target_option_current_node)
    {
      (void) targetm.target_option.pragma_parse (NULL_TREE, new_target);
      target_option_current_node = new_target;
    }

  if (new_optimize != optimization_current_node)
    {
      tree old_optimize = optimization_current_node;
      cl_optimization_restore (&global_options,
			       TREE_OPTIMIZATION (new_optimize));
      c_cpp_builtins_optimize_pragma (parse_in, old_optimize, new_optimize);
      optimization_current_node = new_optimize;
    }

  current_target_pragma = NULL_TREE;
  current_optimize_pragma = NULL_TREE;
}

/* Print a plain user-specified message.  */

static void
handle_pragma_message (cpp_reader *ARG_UNUSED(dummy))
{
  enum cpp_ttype token;
  tree x, message = 0;

  token = pragma_lex (&x);
  if (token == CPP_OPEN_PAREN)
    {
      token = pragma_lex (&x);
      if (token == CPP_STRING)
        message = x;
      else
        GCC_BAD ("expected a string after %<#pragma message%>");
      if (pragma_lex (&x) != CPP_CLOSE_PAREN)
        GCC_BAD ("malformed %<#pragma message%>, ignored");
    }
  else if (token == CPP_STRING)
    message = x;
  else
    GCC_BAD ("expected a string after %<#pragma message%>");

  gcc_assert (message);

  if (pragma_lex (&x) != CPP_EOF)
    warning (OPT_Wpragmas, "junk at end of %<#pragma message%>");

  if (TREE_STRING_LENGTH (message) > 1)
    inform (input_location, "#pragma message: %s", TREE_STRING_POINTER (message));
}

/* Mark whether the current location is valid for a STDC pragma.  */

static bool valid_location_for_stdc_pragma;

void
mark_valid_location_for_stdc_pragma (bool flag)
{
  valid_location_for_stdc_pragma = flag;
}

/* Return true if the current location is valid for a STDC pragma.  */

bool
valid_location_for_stdc_pragma_p (void)
{
  return valid_location_for_stdc_pragma;
}

enum pragma_switch_t { PRAGMA_ON, PRAGMA_OFF, PRAGMA_DEFAULT, PRAGMA_BAD };

/* A STDC pragma must appear outside of external declarations or
   preceding all explicit declarations and statements inside a compound
   statement; its behavior is undefined if used in any other context.
   It takes a switch of ON, OFF, or DEFAULT.  */

static enum pragma_switch_t
handle_stdc_pragma (const char *pname)
{
  const char *arg;
  tree t;
  enum pragma_switch_t ret;

  if (!valid_location_for_stdc_pragma_p ())
    {
      warning (OPT_Wpragmas, "invalid location for %<pragma %s%>, ignored",
	       pname);
      return PRAGMA_BAD;
    }

  if (pragma_lex (&t) != CPP_NAME)
    {
      warning (OPT_Wpragmas, "malformed %<#pragma %s%>, ignored", pname);
      return PRAGMA_BAD;
    }

  arg = IDENTIFIER_POINTER (t);

  if (!strcmp (arg, "ON"))
    ret = PRAGMA_ON;
  else if (!strcmp (arg, "OFF"))
    ret = PRAGMA_OFF;
  else if (!strcmp (arg, "DEFAULT"))
    ret = PRAGMA_DEFAULT;
  else
    {
      warning (OPT_Wpragmas, "malformed %<#pragma %s%>, ignored", pname);
      return PRAGMA_BAD;
    }

  if (pragma_lex (&t) != CPP_EOF)
    {
      warning (OPT_Wpragmas, "junk at end of %<#pragma %s%>", pname);
      return PRAGMA_BAD;
    }

  return ret;
}

/* #pragma STDC FLOAT_CONST_DECIMAL64 ON
   #pragma STDC FLOAT_CONST_DECIMAL64 OFF
   #pragma STDC FLOAT_CONST_DECIMAL64 DEFAULT */

static void
handle_pragma_float_const_decimal64 (cpp_reader *ARG_UNUSED (dummy))
{
  if (c_dialect_cxx ())
    {
      if (warn_unknown_pragmas > in_system_header)
	warning (OPT_Wunknown_pragmas,
		 "%<#pragma STDC FLOAT_CONST_DECIMAL64%> is not supported"
		 " for C++");
      return;
    }

  if (!targetm.decimal_float_supported_p ())
    {
      if (warn_unknown_pragmas > in_system_header)
	warning (OPT_Wunknown_pragmas,
		 "%<#pragma STDC FLOAT_CONST_DECIMAL64%> is not supported"
		 " on this target");
      return;
    }

  pedwarn (input_location, OPT_Wpedantic,
	   "ISO C does not support %<#pragma STDC FLOAT_CONST_DECIMAL64%>");

  switch (handle_stdc_pragma ("STDC FLOAT_CONST_DECIMAL64"))
    {
    case PRAGMA_ON:
      set_float_const_decimal64 ();
      break;
    case PRAGMA_OFF:
    case PRAGMA_DEFAULT:
      clear_float_const_decimal64 ();
      break;
    case PRAGMA_BAD:
      break;
    }
}

/* A vector of registered pragma callbacks, which is never freed.   */

static vec<internal_pragma_handler> registered_pragmas;

typedef struct
{
  const char *space;
  const char *name;
} pragma_ns_name;


static vec<pragma_ns_name> registered_pp_pragmas;

struct omp_pragma_def { const char *name; unsigned int id; };
static const struct omp_pragma_def omp_pragmas[] = {
  { "atomic", PRAGMA_OMP_ATOMIC },
  { "barrier", PRAGMA_OMP_BARRIER },
  { "cancel", PRAGMA_OMP_CANCEL },
  { "cancellation", PRAGMA_OMP_CANCELLATION_POINT },
  { "critical", PRAGMA_OMP_CRITICAL },
  { "declare", PRAGMA_OMP_DECLARE_REDUCTION },
  { "distribute", PRAGMA_OMP_DISTRIBUTE },
  { "end", PRAGMA_OMP_END_DECLARE_TARGET },
  { "flush", PRAGMA_OMP_FLUSH },
  { "for", PRAGMA_OMP_FOR },
  { "master", PRAGMA_OMP_MASTER },
  { "ordered", PRAGMA_OMP_ORDERED },
  { "parallel", PRAGMA_OMP_PARALLEL },
  { "section", PRAGMA_OMP_SECTION },
  { "sections", PRAGMA_OMP_SECTIONS },
  { "simd", PRAGMA_OMP_SIMD },
  { "single", PRAGMA_OMP_SINGLE },
  { "target", PRAGMA_OMP_TARGET },
  { "task", PRAGMA_OMP_TASK },
  { "taskgroup", PRAGMA_OMP_TASKGROUP },
  { "taskwait", PRAGMA_OMP_TASKWAIT },
  { "taskyield", PRAGMA_OMP_TASKYIELD },
  { "teams", PRAGMA_OMP_TEAMS },
  { "threadprivate", PRAGMA_OMP_THREADPRIVATE }
};

void
c_pp_lookup_pragma (unsigned int id, const char **space, const char **name)
{
  const int n_omp_pragmas = sizeof (omp_pragmas) / sizeof (*omp_pragmas);
  int i;

  for (i = 0; i < n_omp_pragmas; ++i)
    if (omp_pragmas[i].id == id)
      {
	*space = "omp";
	*name = omp_pragmas[i].name;
	return;
      }

  if (id >= PRAGMA_FIRST_EXTERNAL
      && (id < PRAGMA_FIRST_EXTERNAL + registered_pp_pragmas.length ()))
    {
      *space = registered_pp_pragmas[id - PRAGMA_FIRST_EXTERNAL].space;
      *name = registered_pp_pragmas[id - PRAGMA_FIRST_EXTERNAL].name;
      return;
    }

  gcc_unreachable ();
}

/* Front-end wrappers for pragma registration to avoid dragging
   cpplib.h in almost everywhere.  */

static void
c_register_pragma_1 (const char *space, const char *name,
                     internal_pragma_handler ihandler, bool allow_expansion)
{
  unsigned id;

  if (flag_preprocess_only)
    {
      pragma_ns_name ns_name;

      if (!allow_expansion)
	return;

      ns_name.space = space;
      ns_name.name = name;
      registered_pp_pragmas.safe_push (ns_name);
      id = registered_pp_pragmas.length ();
      id += PRAGMA_FIRST_EXTERNAL - 1;
    }
  else
    {
      registered_pragmas.safe_push (ihandler);
      id = registered_pragmas.length ();
      id += PRAGMA_FIRST_EXTERNAL - 1;

      /* The C++ front end allocates 6 bits in cp_token; the C front end
	 allocates 7 bits in c_token.  At present this is sufficient.  */
      gcc_assert (id < 64);
    }

  cpp_register_deferred_pragma (parse_in, space, name, id,
				allow_expansion, false);
}

/* Register a C pragma handler, using a space and a name.  It disallows pragma
   expansion (if you want it, use c_register_pragma_with_expansion instead).  */
void
c_register_pragma (const char *space, const char *name,
                   pragma_handler_1arg handler)
{
  internal_pragma_handler ihandler;

  ihandler.handler.handler_1arg = handler;
  ihandler.extra_data = false;
  ihandler.data = NULL;
  c_register_pragma_1 (space, name, ihandler, false);
}

/* Register a C pragma handler, using a space and a name, it also carries an
   extra data field which can be used by the handler.  It disallows pragma
   expansion (if you want it, use c_register_pragma_with_expansion_and_data
   instead).  */
void
c_register_pragma_with_data (const char *space, const char *name,
                             pragma_handler_2arg handler, void * data)
{
  internal_pragma_handler ihandler;

  ihandler.handler.handler_2arg = handler;
  ihandler.extra_data = true;
  ihandler.data = data;
  c_register_pragma_1 (space, name, ihandler, false);
}

/* Register a C pragma handler, using a space and a name.  It allows pragma
   expansion as in the following example:

   #define NUMBER 10
   #pragma count (NUMBER)

   Name expansion is still disallowed.  */
void
c_register_pragma_with_expansion (const char *space, const char *name,
				  pragma_handler_1arg handler)
{
  internal_pragma_handler ihandler;

  ihandler.handler.handler_1arg = handler;
  ihandler.extra_data = false;
  ihandler.data = NULL;
  c_register_pragma_1 (space, name, ihandler, true);
}

/* Register a C pragma handler, using a space and a name, it also carries an
   extra data field which can be used by the handler.  It allows pragma
   expansion as in the following example:

   #define NUMBER 10
   #pragma count (NUMBER)

   Name expansion is still disallowed.  */
void
c_register_pragma_with_expansion_and_data (const char *space, const char *name,
                                           pragma_handler_2arg handler,
                                           void *data)
{
  internal_pragma_handler ihandler;

  ihandler.handler.handler_2arg = handler;
  ihandler.extra_data = true;
  ihandler.data = data;
  c_register_pragma_1 (space, name, ihandler, true);
}

void
c_invoke_pragma_handler (unsigned int id)
{
  internal_pragma_handler *ihandler;
  pragma_handler_1arg handler_1arg;
  pragma_handler_2arg handler_2arg;

  id -= PRAGMA_FIRST_EXTERNAL;
  ihandler = &registered_pragmas[id];
  if (ihandler->extra_data)
    {
      handler_2arg = ihandler->handler.handler_2arg;
      handler_2arg (parse_in, ihandler->data);
    }
  else
    {
      handler_1arg = ihandler->handler.handler_1arg;
      handler_1arg (parse_in);
    }
}

/* Set up front-end pragmas.  */
void
init_pragma (void)
{
  if (flag_openmp)
    {
      const int n_omp_pragmas = sizeof (omp_pragmas) / sizeof (*omp_pragmas);
      int i;

      for (i = 0; i < n_omp_pragmas; ++i)
	cpp_register_deferred_pragma (parse_in, "omp", omp_pragmas[i].name,
				      omp_pragmas[i].id, true, true);
    }

  if (!flag_preprocess_only)
    cpp_register_deferred_pragma (parse_in, "GCC", "pch_preprocess",
				  PRAGMA_GCC_PCH_PREPROCESS, false, false);

#ifdef HANDLE_PRAGMA_PACK_WITH_EXPANSION
  c_register_pragma_with_expansion (0, "pack", handle_pragma_pack);
#else
  c_register_pragma (0, "pack", handle_pragma_pack);
#endif
  c_register_pragma (0, "weak", handle_pragma_weak);
  c_register_pragma ("GCC", "visibility", handle_pragma_visibility);

  c_register_pragma ("GCC", "diagnostic", handle_pragma_diagnostic);
  c_register_pragma ("GCC", "target", handle_pragma_target);
  c_register_pragma ("GCC", "optimize", handle_pragma_optimize);
  c_register_pragma ("GCC", "push_options", handle_pragma_push_options);
  c_register_pragma ("GCC", "pop_options", handle_pragma_pop_options);
  c_register_pragma ("GCC", "reset_options", handle_pragma_reset_options);

  c_register_pragma ("STDC", "FLOAT_CONST_DECIMAL64",
		     handle_pragma_float_const_decimal64);

  c_register_pragma_with_expansion (0, "redefine_extname",
				    handle_pragma_redefine_extname);

  c_register_pragma_with_expansion (0, "message", handle_pragma_message);

#ifdef REGISTER_TARGET_PRAGMAS
  REGISTER_TARGET_PRAGMAS ();
#endif

  /* Allow plugins to register their own pragmas. */
  invoke_plugin_callbacks (PLUGIN_PRAGMAS, NULL);
}

#include "gt-c-family-c-pragma.h"
