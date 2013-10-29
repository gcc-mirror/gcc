/* Handle the hair of processing (but not expanding) inline functions.
   Also manage function and variable name overloading.
   Copyright (C) 1987-2013 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* Handle method declarations.  */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"
#include "toplev.h"
#include "tm_p.h"
#include "target.h"
#include "common/common-target.h"
#include "diagnostic.h"
#include "cgraph.h"
#include "gimple.h"

/* Various flags to control the mangling process.  */

enum mangling_flags
{
  /* No flags.  */
  mf_none = 0,
  /* The thing we are presently mangling is part of a template type,
     rather than a fully instantiated type.  Therefore, we may see
     complex expressions where we would normally expect to see a
     simple integer constant.  */
  mf_maybe_uninstantiated = 1,
  /* When mangling a numeric value, use the form `_XX_' (instead of
     just `XX') if the value has more than one digit.  */
  mf_use_underscores_around_value = 2
};

typedef enum mangling_flags mangling_flags;

static void do_build_copy_assign (tree);
static void do_build_copy_constructor (tree);
static tree make_alias_for_thunk (tree);

/* Called once to initialize method.c.  */

void
init_method (void)
{
  init_mangle ();
}

/* Return a this or result adjusting thunk to FUNCTION.  THIS_ADJUSTING
   indicates whether it is a this or result adjusting thunk.
   FIXED_OFFSET and VIRTUAL_OFFSET indicate how to do the adjustment
   (see thunk_adjust).  VIRTUAL_OFFSET can be NULL, but FIXED_OFFSET
   never is.  VIRTUAL_OFFSET is the /index/ into the vtable for this
   adjusting thunks, we scale it to a byte offset. For covariant
   thunks VIRTUAL_OFFSET is the virtual binfo.  You must post process
   the returned thunk with finish_thunk.  */

tree
make_thunk (tree function, bool this_adjusting,
	    tree fixed_offset, tree virtual_offset)
{
  HOST_WIDE_INT d;
  tree thunk;

  gcc_assert (TREE_CODE (function) == FUNCTION_DECL);
  /* We can have this thunks to covariant thunks, but not vice versa.  */
  gcc_assert (!DECL_THIS_THUNK_P (function));
  gcc_assert (!DECL_RESULT_THUNK_P (function) || this_adjusting);

  /* Scale the VIRTUAL_OFFSET to be in terms of bytes.  */
  if (this_adjusting && virtual_offset)
    virtual_offset
      = size_binop (MULT_EXPR,
		    virtual_offset,
		    convert (ssizetype,
			     TYPE_SIZE_UNIT (vtable_entry_type)));

  d = tree_low_cst (fixed_offset, 0);

  /* See if we already have the thunk in question.  For this_adjusting
     thunks VIRTUAL_OFFSET will be an INTEGER_CST, for covariant thunks it
     will be a BINFO.  */
  for (thunk = DECL_THUNKS (function); thunk; thunk = DECL_CHAIN (thunk))
    if (DECL_THIS_THUNK_P (thunk) == this_adjusting
	&& THUNK_FIXED_OFFSET (thunk) == d
	&& !virtual_offset == !THUNK_VIRTUAL_OFFSET (thunk)
	&& (!virtual_offset
	    || (this_adjusting
		? tree_int_cst_equal (THUNK_VIRTUAL_OFFSET (thunk),
				      virtual_offset)
		: THUNK_VIRTUAL_OFFSET (thunk) == virtual_offset)))
      return thunk;

  /* All thunks must be created before FUNCTION is actually emitted;
     the ABI requires that all thunks be emitted together with the
     function to which they transfer control.  */
  gcc_assert (!TREE_ASM_WRITTEN (function));
  /* Likewise, we can only be adding thunks to a function declared in
     the class currently being laid out.  */
  gcc_assert (TYPE_SIZE (DECL_CONTEXT (function))
	      && TYPE_BEING_DEFINED (DECL_CONTEXT (function)));

  thunk = build_decl (DECL_SOURCE_LOCATION (function),
		      FUNCTION_DECL, NULL_TREE, TREE_TYPE (function));
  DECL_LANG_SPECIFIC (thunk) = DECL_LANG_SPECIFIC (function);
  cxx_dup_lang_specific_decl (thunk);
  DECL_VIRTUAL_P (thunk) = true;
  SET_DECL_THUNKS (thunk, NULL_TREE);

  DECL_CONTEXT (thunk) = DECL_CONTEXT (function);
  TREE_READONLY (thunk) = TREE_READONLY (function);
  TREE_THIS_VOLATILE (thunk) = TREE_THIS_VOLATILE (function);
  TREE_PUBLIC (thunk) = TREE_PUBLIC (function);
  SET_DECL_THUNK_P (thunk, this_adjusting);
  THUNK_TARGET (thunk) = function;
  THUNK_FIXED_OFFSET (thunk) = d;
  THUNK_VIRTUAL_OFFSET (thunk) = virtual_offset;
  THUNK_ALIAS (thunk) = NULL_TREE;

  DECL_INTERFACE_KNOWN (thunk) = 1;
  DECL_NOT_REALLY_EXTERN (thunk) = 1;
  DECL_COMDAT (thunk) = DECL_COMDAT (function);
  DECL_SAVED_FUNCTION_DATA (thunk) = NULL;
  /* The thunk itself is not a constructor or destructor, even if
     the thing it is thunking to is.  */
  DECL_DESTRUCTOR_P (thunk) = 0;
  DECL_CONSTRUCTOR_P (thunk) = 0;
  DECL_EXTERNAL (thunk) = 1;
  DECL_ARTIFICIAL (thunk) = 1;
  /* The THUNK is not a pending inline, even if the FUNCTION is.  */
  DECL_PENDING_INLINE_P (thunk) = 0;
  DECL_DECLARED_INLINE_P (thunk) = 0;
  /* Nor is it a template instantiation.  */
  DECL_USE_TEMPLATE (thunk) = 0;
  DECL_TEMPLATE_INFO (thunk) = NULL;

  /* Add it to the list of thunks associated with FUNCTION.  */
  DECL_CHAIN (thunk) = DECL_THUNKS (function);
  SET_DECL_THUNKS (function, thunk);

  return thunk;
}

/* Finish THUNK, a thunk decl.  */

void
finish_thunk (tree thunk)
{
  tree function, name;
  tree fixed_offset = ssize_int (THUNK_FIXED_OFFSET (thunk));
  tree virtual_offset = THUNK_VIRTUAL_OFFSET (thunk);

  gcc_assert (!DECL_NAME (thunk) && DECL_THUNK_P (thunk));
  if (virtual_offset && DECL_RESULT_THUNK_P (thunk))
    virtual_offset = BINFO_VPTR_FIELD (virtual_offset);
  function = THUNK_TARGET (thunk);
  name = mangle_thunk (function, DECL_THIS_THUNK_P (thunk),
		       fixed_offset, virtual_offset);

  /* We can end up with declarations of (logically) different
     covariant thunks, that do identical adjustments.  The two thunks
     will be adjusting between within different hierarchies, which
     happen to have the same layout.  We must nullify one of them to
     refer to the other.  */
  if (DECL_RESULT_THUNK_P (thunk))
    {
      tree cov_probe;

      for (cov_probe = DECL_THUNKS (function);
	   cov_probe; cov_probe = DECL_CHAIN (cov_probe))
	if (DECL_NAME (cov_probe) == name)
	  {
	    gcc_assert (!DECL_THUNKS (thunk));
	    THUNK_ALIAS (thunk) = (THUNK_ALIAS (cov_probe)
				   ? THUNK_ALIAS (cov_probe) : cov_probe);
	    break;
	  }
    }

  DECL_NAME (thunk) = name;
  SET_DECL_ASSEMBLER_NAME (thunk, name);
}

static GTY (()) int thunk_labelno;

/* Create a static alias to target.  */

tree
make_alias_for (tree target, tree newid)
{
  tree alias = build_decl (DECL_SOURCE_LOCATION (target),
			   TREE_CODE (target), newid, TREE_TYPE (target));
  DECL_LANG_SPECIFIC (alias) = DECL_LANG_SPECIFIC (target);
  cxx_dup_lang_specific_decl (alias);
  DECL_CONTEXT (alias) = NULL;
  TREE_READONLY (alias) = TREE_READONLY (target);
  TREE_THIS_VOLATILE (alias) = TREE_THIS_VOLATILE (target);
  TREE_PUBLIC (alias) = 0;
  DECL_INTERFACE_KNOWN (alias) = 1;
  if (DECL_LANG_SPECIFIC (alias))
    {
      DECL_NOT_REALLY_EXTERN (alias) = 1;
      DECL_USE_TEMPLATE (alias) = 0;
      DECL_TEMPLATE_INFO (alias) = NULL;
    }
  DECL_EXTERNAL (alias) = 0;
  DECL_ARTIFICIAL (alias) = 1;
  DECL_TEMPLATE_INSTANTIATED (alias) = 0;
  if (TREE_CODE (alias) == FUNCTION_DECL)
    {
      DECL_SAVED_FUNCTION_DATA (alias) = NULL;
      DECL_DESTRUCTOR_P (alias) = 0;
      DECL_CONSTRUCTOR_P (alias) = 0;
      DECL_PENDING_INLINE_P (alias) = 0;
      DECL_DECLARED_INLINE_P (alias) = 0;
      DECL_INITIAL (alias) = error_mark_node;
      DECL_ARGUMENTS (alias) = copy_list (DECL_ARGUMENTS (target));
    }
  else
    TREE_STATIC (alias) = 1;
  TREE_ADDRESSABLE (alias) = 1;
  TREE_USED (alias) = 1;
  SET_DECL_ASSEMBLER_NAME (alias, DECL_NAME (alias));
  return alias;
}

static tree
make_alias_for_thunk (tree function)
{
  tree alias;
  char buf[256];

  targetm.asm_out.generate_internal_label (buf, "LTHUNK", thunk_labelno);
  thunk_labelno++;

  alias = make_alias_for (function, get_identifier (buf));

  if (!flag_syntax_only)
    {
      struct cgraph_node *funcn, *aliasn;
      funcn = cgraph_get_node (function);
      gcc_checking_assert (funcn);
      aliasn = cgraph_same_body_alias (funcn, alias, function);
      DECL_ASSEMBLER_NAME (function);
      gcc_assert (aliasn != NULL);
    }

  return alias;
}

/* Emit the definition of a C++ multiple inheritance or covariant
   return vtable thunk.  If EMIT_P is nonzero, the thunk is emitted
   immediately.  */

void
use_thunk (tree thunk_fndecl, bool emit_p)
{
  tree a, t, function, alias;
  tree virtual_offset;
  HOST_WIDE_INT fixed_offset, virtual_value;
  bool this_adjusting = DECL_THIS_THUNK_P (thunk_fndecl);
  struct cgraph_node *funcn, *thunk_node;

  /* We should have called finish_thunk to give it a name.  */
  gcc_assert (DECL_NAME (thunk_fndecl));

  /* We should never be using an alias, always refer to the
     aliased thunk.  */
  gcc_assert (!THUNK_ALIAS (thunk_fndecl));

  if (TREE_ASM_WRITTEN (thunk_fndecl))
    return;

  function = THUNK_TARGET (thunk_fndecl);
  if (DECL_RESULT (thunk_fndecl))
    /* We already turned this thunk into an ordinary function.
       There's no need to process this thunk again.  */
    return;

  if (DECL_THUNK_P (function))
    /* The target is itself a thunk, process it now.  */
    use_thunk (function, emit_p);

  /* Thunks are always addressable; they only appear in vtables.  */
  TREE_ADDRESSABLE (thunk_fndecl) = 1;

  /* Figure out what function is being thunked to.  It's referenced in
     this translation unit.  */
  TREE_ADDRESSABLE (function) = 1;
  mark_used (function);
  if (!emit_p)
    return;

  if (TARGET_USE_LOCAL_THUNK_ALIAS_P (function))
   alias = make_alias_for_thunk (function);
  else
   alias = function;

  fixed_offset = THUNK_FIXED_OFFSET (thunk_fndecl);
  virtual_offset = THUNK_VIRTUAL_OFFSET (thunk_fndecl);

  if (virtual_offset)
    {
      if (!this_adjusting)
	virtual_offset = BINFO_VPTR_FIELD (virtual_offset);
      virtual_value = tree_low_cst (virtual_offset, /*pos=*/0);
      gcc_assert (virtual_value);
    }
  else
    virtual_value = 0;

  /* And, if we need to emit the thunk, it's used.  */
  mark_used (thunk_fndecl);
  /* This thunk is actually defined.  */
  DECL_EXTERNAL (thunk_fndecl) = 0;
  /* The linkage of the function may have changed.  FIXME in linkage
     rewrite.  */
  gcc_assert (DECL_INTERFACE_KNOWN (function));
  TREE_PUBLIC (thunk_fndecl) = TREE_PUBLIC (function);
  DECL_VISIBILITY (thunk_fndecl) = DECL_VISIBILITY (function);
  DECL_VISIBILITY_SPECIFIED (thunk_fndecl)
    = DECL_VISIBILITY_SPECIFIED (function);
  DECL_COMDAT (thunk_fndecl) = DECL_COMDAT (function);
  DECL_WEAK (thunk_fndecl) = DECL_WEAK (function);

  if (flag_syntax_only)
    {
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      return;
    }

  push_to_top_level ();

  if (TARGET_USE_LOCAL_THUNK_ALIAS_P (function)
      && targetm_common.have_named_sections)
    {
      resolve_unique_section (function, 0, flag_function_sections);

      if (DECL_SECTION_NAME (function) != NULL && DECL_ONE_ONLY (function))
	{
	  resolve_unique_section (thunk_fndecl, 0, flag_function_sections);

	  /* Output the thunk into the same section as function.  */
	  DECL_SECTION_NAME (thunk_fndecl) = DECL_SECTION_NAME (function);
	}
    }

  /* Set up cloned argument trees for the thunk.  */
  t = NULL_TREE;
  for (a = DECL_ARGUMENTS (function); a; a = DECL_CHAIN (a))
    {
      tree x = copy_node (a);
      DECL_CHAIN (x) = t;
      DECL_CONTEXT (x) = thunk_fndecl;
      SET_DECL_RTL (x, NULL);
      DECL_HAS_VALUE_EXPR_P (x) = 0;
      TREE_ADDRESSABLE (x) = 0;
      t = x;
    }
  a = nreverse (t);
  DECL_ARGUMENTS (thunk_fndecl) = a;
  TREE_ASM_WRITTEN (thunk_fndecl) = 1;
  funcn = cgraph_get_node (function);
  gcc_checking_assert (funcn);
  thunk_node = cgraph_add_thunk (funcn, thunk_fndecl, function,
				 this_adjusting, fixed_offset, virtual_value,
				 virtual_offset, alias);
  if (DECL_ONE_ONLY (function))
    symtab_add_to_same_comdat_group (thunk_node,
				     funcn);

  if (!this_adjusting
      || !targetm.asm_out.can_output_mi_thunk (thunk_fndecl, fixed_offset,
					       virtual_value, alias))
    {
      /* If this is a covariant thunk, or we don't have the necessary
	 code for efficient thunks, generate a thunk function that
	 just makes a call to the real function.  Unfortunately, this
	 doesn't work for varargs.  */

      if (varargs_function_p (function))
	error ("generic thunk code fails for method %q#D which uses %<...%>",
	       function);
    }

  pop_from_top_level ();
}

/* Code for synthesizing methods which have default semantics defined.  */

/* True iff CTYPE has a trivial SFK.  */

static bool
type_has_trivial_fn (tree ctype, special_function_kind sfk)
{
  switch (sfk)
    {
    case sfk_constructor:
      return !TYPE_HAS_COMPLEX_DFLT (ctype);
    case sfk_copy_constructor:
      return !TYPE_HAS_COMPLEX_COPY_CTOR (ctype);
    case sfk_move_constructor:
      return !TYPE_HAS_COMPLEX_MOVE_CTOR (ctype);
    case sfk_copy_assignment:
      return !TYPE_HAS_COMPLEX_COPY_ASSIGN (ctype);
    case sfk_move_assignment:
      return !TYPE_HAS_COMPLEX_MOVE_ASSIGN (ctype);
    case sfk_destructor:
      return !TYPE_HAS_NONTRIVIAL_DESTRUCTOR (ctype);
    case sfk_inheriting_constructor:
      return false;
    default:
      gcc_unreachable ();
    }
}

/* Note that CTYPE has a non-trivial SFK even though we previously thought
   it was trivial.  */

static void
type_set_nontrivial_flag (tree ctype, special_function_kind sfk)
{
  switch (sfk)
    {
    case sfk_constructor:
      TYPE_HAS_COMPLEX_DFLT (ctype) = true;
      return;
    case sfk_copy_constructor:
      TYPE_HAS_COMPLEX_COPY_CTOR (ctype) = true;
      return;
    case sfk_move_constructor:
      TYPE_HAS_COMPLEX_MOVE_CTOR (ctype) = true;
      return;
    case sfk_copy_assignment:
      TYPE_HAS_COMPLEX_COPY_ASSIGN (ctype) = true;
      return;
    case sfk_move_assignment:
      TYPE_HAS_COMPLEX_MOVE_ASSIGN (ctype) = true;
      return;
    case sfk_destructor:
      TYPE_HAS_NONTRIVIAL_DESTRUCTOR (ctype) = true;
      return;
    case sfk_inheriting_constructor:
    default:
      gcc_unreachable ();
    }
}

/* True iff FN is a trivial defaulted member function ([cd]tor, op=).  */

bool
trivial_fn_p (tree fn)
{
  if (!DECL_DEFAULTED_FN (fn))
    return false;

  /* If fn is a clone, get the primary variant.  */
  fn = DECL_ORIGIN (fn);
  return type_has_trivial_fn (DECL_CONTEXT (fn), special_function_p (fn));
}

/* Subroutine of do_build_copy_constructor: Add a mem-initializer for BINFO
   given the parameter or parameters PARM, possibly inherited constructor
   base INH, or move flag MOVE_P.  */

static tree
add_one_base_init (tree binfo, tree parm, bool move_p, tree inh,
		   tree member_init_list)
{
  tree init;
  if (inh)
    {
      /* An inheriting constructor only has a mem-initializer for
	 the base it inherits from.  */
      if (BINFO_TYPE (binfo) != inh)
	return member_init_list;

      tree *p = &init;
      init = NULL_TREE;
      for (; parm; parm = DECL_CHAIN (parm))
	{
	  tree exp = convert_from_reference (parm);
	  if (TREE_CODE (TREE_TYPE (parm)) != REFERENCE_TYPE
	      || TYPE_REF_IS_RVALUE (TREE_TYPE (parm)))
	    exp = move (exp);
	  *p = build_tree_list (NULL_TREE, exp);
	  p = &TREE_CHAIN (*p);
	}
    }
  else
    {
      init = build_base_path (PLUS_EXPR, parm, binfo, 1,
			      tf_warning_or_error);
      if (move_p)
	init = move (init);
      init = build_tree_list (NULL_TREE, init);
    }
  return tree_cons (binfo, init, member_init_list);
}

/* Generate code for default X(X&) or X(X&&) constructor or an inheriting
   constructor.  */

static void
do_build_copy_constructor (tree fndecl)
{
  tree parm = FUNCTION_FIRST_USER_PARM (fndecl);
  bool move_p = DECL_MOVE_CONSTRUCTOR_P (fndecl);
  bool trivial = trivial_fn_p (fndecl);
  tree inh = DECL_INHERITED_CTOR_BASE (fndecl);

  if (!inh)
    parm = convert_from_reference (parm);

  if (trivial
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (trivial)
    {
      tree t = build2 (INIT_EXPR, void_type_node, current_class_ref, parm);
      finish_expr_stmt (t);
    }
  else
    {
      tree fields = TYPE_FIELDS (current_class_type);
      tree member_init_list = NULL_TREE;
      int cvquals = cp_type_quals (TREE_TYPE (parm));
      int i;
      tree binfo, base_binfo;
      tree init;
      vec<tree, va_gc> *vbases;

      /* Initialize all the base-classes with the parameter converted
	 to their type so that we get their copy constructor and not
	 another constructor that takes current_class_type.  We must
	 deal with the binfo's directly as a direct base might be
	 inaccessible due to ambiguity.  */
      for (vbases = CLASSTYPE_VBASECLASSES (current_class_type), i = 0;
	   vec_safe_iterate (vbases, i, &binfo); i++)
	{
	  member_init_list = add_one_base_init (binfo, parm, move_p, inh,
						member_init_list);
	}

      for (binfo = TYPE_BINFO (current_class_type), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	{
	  if (BINFO_VIRTUAL_P (base_binfo))
	    continue;
	  member_init_list = add_one_base_init (base_binfo, parm, move_p,
						inh, member_init_list);
	}

      for (; fields; fields = DECL_CHAIN (fields))
	{
	  tree field = fields;
	  tree expr_type;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  if (inh)
	    continue;

	  expr_type = TREE_TYPE (field);
	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;
	    }
	  else if (ANON_AGGR_TYPE_P (expr_type) && TYPE_FIELDS (expr_type))
	    /* Just use the field; anonymous types can't have
	       nontrivial copy ctors or assignment ops or this
	       function would be deleted.  */;
	  else
	    continue;

	  /* Compute the type of "init->field".  If the copy-constructor
	     parameter is, for example, "const S&", and the type of
	     the field is "T", then the type will usually be "const
	     T".  (There are no cv-qualified variants of reference
	     types.)  */
	  if (TREE_CODE (expr_type) != REFERENCE_TYPE)
	    {
	      int quals = cvquals;

	      if (DECL_MUTABLE_P (field))
		quals &= ~TYPE_QUAL_CONST;
	      quals |= cp_type_quals (expr_type);
	      expr_type = cp_build_qualified_type (expr_type, quals);
	    }

	  init = build3 (COMPONENT_REF, expr_type, parm, field, NULL_TREE);
	  if (move_p && TREE_CODE (expr_type) != REFERENCE_TYPE
	      /* 'move' breaks bit-fields, and has no effect for scalars.  */
	      && !scalarish_type_p (expr_type))
	    init = move (init);
	  init = build_tree_list (NULL_TREE, init);

	  member_init_list = tree_cons (field, init, member_init_list);
	}
      finish_mem_initializers (member_init_list);
    }
}

static void
do_build_copy_assign (tree fndecl)
{
  tree parm = DECL_CHAIN (DECL_ARGUMENTS (fndecl));
  tree compound_stmt;
  bool move_p = move_fn_p (fndecl);
  bool trivial = trivial_fn_p (fndecl);
  int flags = LOOKUP_NORMAL | LOOKUP_NONVIRTUAL | LOOKUP_DEFAULTED;

  compound_stmt = begin_compound_stmt (0);
  parm = convert_from_reference (parm);

  if (trivial
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (trivial)
    {
      tree t = build2 (MODIFY_EXPR, void_type_node, current_class_ref, parm);
      finish_expr_stmt (t);
    }
  else
    {
      tree fields;
      int cvquals = cp_type_quals (TREE_TYPE (parm));
      int i;
      tree binfo, base_binfo;

      /* Assign to each of the direct base classes.  */
      for (binfo = TYPE_BINFO (current_class_type), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	{
	  tree converted_parm;
	  vec<tree, va_gc> *parmvec;

	  /* We must convert PARM directly to the base class
	     explicitly since the base class may be ambiguous.  */
	  converted_parm = build_base_path (PLUS_EXPR, parm, base_binfo, 1,
					    tf_warning_or_error);
	  if (move_p)
	    converted_parm = move (converted_parm);
	  /* Call the base class assignment operator.  */
	  parmvec = make_tree_vector_single (converted_parm);
	  finish_expr_stmt
	    (build_special_member_call (current_class_ref,
					ansi_assopname (NOP_EXPR),
					&parmvec,
					base_binfo,
					flags,
                                        tf_warning_or_error));
	  release_tree_vector (parmvec);
	}

      /* Assign to each of the non-static data members.  */
      for (fields = TYPE_FIELDS (current_class_type);
	   fields;
	   fields = DECL_CHAIN (fields))
	{
	  tree comp = current_class_ref;
	  tree init = parm;
	  tree field = fields;
	  tree expr_type;
	  int quals;

	  if (TREE_CODE (field) != FIELD_DECL || DECL_ARTIFICIAL (field))
	    continue;

	  expr_type = TREE_TYPE (field);

	  if (CP_TYPE_CONST_P (expr_type))
	    {
	      error ("non-static const member %q#D, can%'t use default "
		     "assignment operator", field);
	      continue;
	    }
	  else if (TREE_CODE (expr_type) == REFERENCE_TYPE)
	    {
	      error ("non-static reference member %q#D, can%'t use "
		     "default assignment operator", field);
	      continue;
	    }

	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;
	    }
	  else if (ANON_AGGR_TYPE_P (expr_type)
		   && TYPE_FIELDS (expr_type) != NULL_TREE)
	    /* Just use the field; anonymous types can't have
	       nontrivial copy ctors or assignment ops or this
	       function would be deleted.  */;
	  else
	    continue;

	  comp = build3 (COMPONENT_REF, expr_type, comp, field, NULL_TREE);

	  /* Compute the type of init->field  */
	  quals = cvquals;
	  if (DECL_MUTABLE_P (field))
	    quals &= ~TYPE_QUAL_CONST;
	  expr_type = cp_build_qualified_type (expr_type, quals);

	  init = build3 (COMPONENT_REF, expr_type, init, field, NULL_TREE);
	  if (move_p && TREE_CODE (expr_type) != REFERENCE_TYPE
	      /* 'move' breaks bit-fields, and has no effect for scalars.  */
	      && !scalarish_type_p (expr_type))
	    init = move (init);

	  if (DECL_NAME (field))
	    init = cp_build_modify_expr (comp, NOP_EXPR, init, 
					 tf_warning_or_error);
	  else
	    init = build2 (MODIFY_EXPR, TREE_TYPE (comp), comp, init);
	  finish_expr_stmt (init);
	}
    }
  finish_return_stmt (current_class_ref);
  finish_compound_stmt (compound_stmt);
}

/* Synthesize FNDECL, a non-static member function.   */

void
synthesize_method (tree fndecl)
{
  bool nested = (current_function_decl != NULL_TREE);
  tree context = decl_function_context (fndecl);
  bool need_body = true;
  tree stmt;
  location_t save_input_location = input_location;
  int error_count = errorcount;
  int warning_count = warningcount + werrorcount;

  /* Reset the source location, we might have been previously
     deferred, and thus have saved where we were first needed.  */
  DECL_SOURCE_LOCATION (fndecl)
    = DECL_SOURCE_LOCATION (TYPE_NAME (DECL_CONTEXT (fndecl)));

  /* If we've been asked to synthesize a clone, just synthesize the
     cloned function instead.  Doing so will automatically fill in the
     body for the clone.  */
  if (DECL_CLONED_FUNCTION_P (fndecl))
    fndecl = DECL_CLONED_FUNCTION (fndecl);

  /* We may be in the middle of deferred access check.  Disable
     it now.  */
  push_deferring_access_checks (dk_no_deferred);

  if (! context)
    push_to_top_level ();
  else if (nested)
    push_function_context ();

  input_location = DECL_SOURCE_LOCATION (fndecl);

  start_preparsed_function (fndecl, NULL_TREE, SF_DEFAULT | SF_PRE_PARSED);
  stmt = begin_function_body ();

  if (DECL_OVERLOADED_OPERATOR_P (fndecl) == NOP_EXPR)
    {
      do_build_copy_assign (fndecl);
      need_body = false;
    }
  else if (DECL_CONSTRUCTOR_P (fndecl))
    {
      tree arg_chain = FUNCTION_FIRST_USER_PARMTYPE (fndecl);
      if (arg_chain != void_list_node)
	do_build_copy_constructor (fndecl);
      else
	finish_mem_initializers (NULL_TREE);
    }

  /* If we haven't yet generated the body of the function, just
     generate an empty compound statement.  */
  if (need_body)
    {
      tree compound_stmt;
      compound_stmt = begin_compound_stmt (BCS_FN_BODY);
      finish_compound_stmt (compound_stmt);
    }

  finish_function_body (stmt);
  expand_or_defer_fn (finish_function (0));

  input_location = save_input_location;

  if (! context)
    pop_from_top_level ();
  else if (nested)
    pop_function_context ();

  pop_deferring_access_checks ();

  if (error_count != errorcount || warning_count != warningcount + werrorcount)
    inform (input_location, "synthesized method %qD first required here ",
	    fndecl);
}

/* Build a reference to type TYPE with cv-quals QUALS, which is an
   rvalue if RVALUE is true.  */

static tree
build_stub_type (tree type, int quals, bool rvalue)
{
  tree argtype = cp_build_qualified_type (type, quals);
  return cp_build_reference_type (argtype, rvalue);
}

/* Build a dummy glvalue from dereferencing a dummy reference of type
   REFTYPE.  */

static tree
build_stub_object (tree reftype)
{
  tree stub = build1 (NOP_EXPR, reftype, integer_one_node);
  return convert_from_reference (stub);
}

/* Determine which function will be called when looking up NAME in TYPE,
   called with a single ARGTYPE argument, or no argument if ARGTYPE is
   null.  FLAGS and COMPLAIN are as for build_new_method_call.

   Returns a FUNCTION_DECL if all is well.
   Returns NULL_TREE if overload resolution failed.
   Returns error_mark_node if the chosen function cannot be called.  */

static tree
locate_fn_flags (tree type, tree name, tree argtype, int flags,
		 tsubst_flags_t complain)
{
  tree ob, fn, fns, binfo, rval;
  vec<tree, va_gc> *args;

  if (TYPE_P (type))
    binfo = TYPE_BINFO (type);
  else
    {
      binfo = type;
      type = BINFO_TYPE (binfo);
    }

  ob = build_stub_object (cp_build_reference_type (type, false));
  args = make_tree_vector ();
  if (argtype)
    {
      if (TREE_CODE (argtype) == TREE_LIST)
	{
	  for (tree elt = argtype; elt != void_list_node;
	       elt = TREE_CHAIN (elt))
	    {
	      tree type = TREE_VALUE (elt);
	      if (TREE_CODE (type) != REFERENCE_TYPE)
		type = cp_build_reference_type (type, /*rval*/true);
	      tree arg = build_stub_object (type);
	      vec_safe_push (args, arg);
	    }
	}
      else
	{
	  tree arg = build_stub_object (argtype);
	  args->quick_push (arg);
	}
    }

  fns = lookup_fnfields (binfo, name, 0);
  rval = build_new_method_call (ob, fns, &args, binfo, flags, &fn, complain);

  release_tree_vector (args);
  if (fn && rval == error_mark_node)
    return rval;
  else
    return fn;
}

/* Locate the dtor of TYPE.  */

tree
get_dtor (tree type, tsubst_flags_t complain)
{
  tree fn = locate_fn_flags (type, complete_dtor_identifier, NULL_TREE,
			     LOOKUP_NORMAL, complain);
  if (fn == error_mark_node)
    return NULL_TREE;
  return fn;
}

/* Locate the default ctor of TYPE.  */

tree
locate_ctor (tree type)
{
  tree fn;

  push_deferring_access_checks (dk_no_check);
  fn = locate_fn_flags (type, complete_ctor_identifier, NULL_TREE,
			LOOKUP_SPECULATIVE, tf_none);
  pop_deferring_access_checks ();
  if (fn == error_mark_node)
    return NULL_TREE;
  return fn;
}

/* Likewise, but give any appropriate errors.  */

tree
get_default_ctor (tree type)
{
  tree fn = locate_fn_flags (type, complete_ctor_identifier, NULL_TREE,
			     LOOKUP_NORMAL, tf_warning_or_error);
  if (fn == error_mark_node)
    return NULL_TREE;
  return fn;
}

/* Locate the copy ctor of TYPE.  */

tree
get_copy_ctor (tree type, tsubst_flags_t complain)
{
  int quals = (TYPE_HAS_CONST_COPY_CTOR (type)
	       ? TYPE_QUAL_CONST : TYPE_UNQUALIFIED);
  tree argtype = build_stub_type (type, quals, false);
  tree fn = locate_fn_flags (type, complete_ctor_identifier, argtype,
			     LOOKUP_NORMAL, complain);
  if (fn == error_mark_node)
    return NULL_TREE;
  return fn;
}

/* Locate the copy assignment operator of TYPE.  */

tree
get_copy_assign (tree type)
{
  int quals = (TYPE_HAS_CONST_COPY_ASSIGN (type)
	       ? TYPE_QUAL_CONST : TYPE_UNQUALIFIED);
  tree argtype = build_stub_type (type, quals, false);
  tree fn = locate_fn_flags (type, ansi_assopname (NOP_EXPR), argtype,
			     LOOKUP_NORMAL, tf_warning_or_error);
  if (fn == error_mark_node)
    return NULL_TREE;
  return fn;
}

/* Subroutine of synthesized_method_walk.  Update SPEC_P, TRIVIAL_P and
   DELETED_P or give an error message MSG with argument ARG.  */

static void
process_subob_fn (tree fn, tree *spec_p, bool *trivial_p,
		  bool *deleted_p, bool *constexpr_p,
		  bool diag, tree arg)
{
  if (!fn || fn == error_mark_node)
    goto bad;

  if (spec_p)
    {
      tree raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn));
      *spec_p = merge_exception_specifiers (*spec_p, raises, fn);
    }

  if (!trivial_fn_p (fn))
    {
      if (trivial_p)
	*trivial_p = false;
      if (TREE_CODE (arg) == FIELD_DECL
	  && TREE_CODE (DECL_CONTEXT (arg)) == UNION_TYPE)
	{
	  if (deleted_p)
	    *deleted_p = true;
	  if (diag)
	    error ("union member %q+D with non-trivial %qD", arg, fn);
	}
    }

  if (constexpr_p && !DECL_DECLARED_CONSTEXPR_P (fn))
    {
      *constexpr_p = false;
      if (diag)
	{
	  inform (0, "defaulted constructor calls non-constexpr "
		  "%q+D", fn);
	  explain_invalid_constexpr_fn (fn);
	}
    }

  return;

 bad:
  if (deleted_p)
    *deleted_p = true;
}

/* Subroutine of synthesized_method_walk to allow recursion into anonymous
   aggregates.  */

static void
walk_field_subobs (tree fields, tree fnname, special_function_kind sfk,
		   int quals, bool copy_arg_p, bool move_p,
		   bool assign_p, tree *spec_p, bool *trivial_p,
		   bool *deleted_p, bool *constexpr_p,
		   bool diag, int flags, tsubst_flags_t complain)
{
  tree field;
  for (field = fields; field; field = DECL_CHAIN (field))
    {
      tree mem_type, argtype, rval;

      if (TREE_CODE (field) != FIELD_DECL
	  || DECL_ARTIFICIAL (field))
	continue;

      mem_type = strip_array_types (TREE_TYPE (field));
      if (assign_p)
	{
	  bool bad = true;
	  if (CP_TYPE_CONST_P (mem_type) && !CLASS_TYPE_P (mem_type))
	    {
	      if (diag)
		error ("non-static const member %q#D, can%'t use default "
		       "assignment operator", field);
	    }
	  else if (TREE_CODE (mem_type) == REFERENCE_TYPE)
	    {
	      if (diag)
		error ("non-static reference member %q#D, can%'t use "
		       "default assignment operator", field);
	    }
	  else
	    bad = false;

	  if (bad && deleted_p)
	    *deleted_p = true;
	}
      else if (sfk == sfk_constructor)
	{
	  bool bad;

	  if (DECL_INITIAL (field))
	    {
	      if (diag && DECL_INITIAL (field) == error_mark_node)
		inform (0, "initializer for %q+#D is invalid", field);
	      if (trivial_p)
		*trivial_p = false;
#if 0
	      /* Core 1351: If the field has an NSDMI that could throw, the
		 default constructor is noexcept(false).  FIXME this is
		 broken by deferred parsing and 1360 saying we can't lazily
		 declare a non-trivial default constructor.  Also this
		 needs to do deferred instantiation.  Disable until the
		 conflict between 1351 and 1360 is resolved.  */
	      if (spec_p && !expr_noexcept_p (DECL_INITIAL (field), complain))
		*spec_p = noexcept_false_spec;
#endif

	      /* Don't do the normal processing.  */
	      continue;
	    }

	  bad = false;
	  if (CP_TYPE_CONST_P (mem_type)
	      && default_init_uninitialized_part (mem_type))
	    {
	      if (diag)
		error ("uninitialized non-static const member %q#D",
		       field);
	      bad = true;
	    }
	  else if (TREE_CODE (mem_type) == REFERENCE_TYPE)
	    {
	      if (diag)
		error ("uninitialized non-static reference member %q#D",
		       field);
	      bad = true;
	    }

	  if (bad && deleted_p)
	    *deleted_p = true;

	  /* For an implicitly-defined default constructor to be constexpr,
	     every member must have a user-provided default constructor or
	     an explicit initializer.  */
	  if (constexpr_p && !CLASS_TYPE_P (mem_type)
	      && TREE_CODE (DECL_CONTEXT (field)) != UNION_TYPE)
	    {
	      *constexpr_p = false;
	      if (diag)
		inform (0, "defaulted default constructor does not "
			"initialize %q+#D", field);
	    }
	}
      else if (sfk == sfk_copy_constructor)
	{
	  /* 12.8p11b5 */
	  if (TREE_CODE (mem_type) == REFERENCE_TYPE
	      && TYPE_REF_IS_RVALUE (mem_type))
	    {
	      if (diag)
		error ("copying non-static data member %q#D of rvalue "
		       "reference type", field);
	      if (deleted_p)
		*deleted_p = true;
	    }
	}

      if (!CLASS_TYPE_P (mem_type))
	continue;

      if (ANON_AGGR_TYPE_P (mem_type))
	{
	  walk_field_subobs (TYPE_FIELDS (mem_type), fnname, sfk, quals,
			     copy_arg_p, move_p, assign_p, spec_p, trivial_p,
			     deleted_p, constexpr_p,
			     diag, flags, complain);
	  continue;
	}

      if (copy_arg_p)
	{
	  int mem_quals = cp_type_quals (mem_type) | quals;
	  if (DECL_MUTABLE_P (field))
	    mem_quals &= ~TYPE_QUAL_CONST;
	  argtype = build_stub_type (mem_type, mem_quals, move_p);
	}
      else
	argtype = NULL_TREE;

      rval = locate_fn_flags (mem_type, fnname, argtype, flags, complain);

      process_subob_fn (rval, spec_p, trivial_p, deleted_p,
			constexpr_p, diag, field);
    }
}

/* The caller wants to generate an implicit declaration of SFK for CTYPE
   which is const if relevant and CONST_P is set.  If spec_p, trivial_p and
   deleted_p are non-null, set their referent appropriately.  If diag is
   true, we're either being called from maybe_explain_implicit_delete to
   give errors, or if constexpr_p is non-null, from
   explain_invalid_constexpr_fn.  */

static void
synthesized_method_walk (tree ctype, special_function_kind sfk, bool const_p,
			 tree *spec_p, bool *trivial_p, bool *deleted_p,
			 bool *constexpr_p, bool diag,
			 tree inherited_base, tree inherited_parms)
{
  tree binfo, base_binfo, scope, fnname, rval, argtype;
  bool move_p, copy_arg_p, assign_p, expected_trivial, check_vdtor;
  vec<tree, va_gc> *vbases;
  int i, quals, flags;
  tsubst_flags_t complain;
  bool ctor_p;

  if (spec_p)
    *spec_p = (cxx_dialect >= cxx11 ? noexcept_true_spec : empty_except_spec);

  if (deleted_p)
    {
      /* "The closure type associated with a lambda-expression has a deleted
	 default constructor and a deleted copy assignment operator."
         This is diagnosed in maybe_explain_implicit_delete.  */
      if (LAMBDA_TYPE_P (ctype)
	  && (sfk == sfk_constructor
	      || sfk == sfk_copy_assignment))
	{
	  *deleted_p = true;
	  return;
	}

      *deleted_p = false;
    }

  ctor_p = false;
  assign_p = false;
  check_vdtor = false;
  switch (sfk)
    {
    case sfk_move_assignment:
    case sfk_copy_assignment:
      assign_p = true;
      fnname = ansi_assopname (NOP_EXPR);
      break;

    case sfk_destructor:
      check_vdtor = true;
      /* The synthesized method will call base dtors, but check complete
	 here to avoid having to deal with VTT.  */
      fnname = complete_dtor_identifier;
      break;

    case sfk_constructor:
    case sfk_move_constructor:
    case sfk_copy_constructor:
    case sfk_inheriting_constructor:
      ctor_p = true;
      fnname = complete_ctor_identifier;
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert ((sfk == sfk_inheriting_constructor)
	      == (inherited_base != NULL_TREE));

  /* If that user-written default constructor would satisfy the
     requirements of a constexpr constructor (7.1.5), the
     implicitly-defined default constructor is constexpr.  */
  if (constexpr_p)
    *constexpr_p = ctor_p;

  move_p = false;
  switch (sfk)
    {
    case sfk_constructor:
    case sfk_destructor:
    case sfk_inheriting_constructor:
      copy_arg_p = false;
      break;

    case sfk_move_constructor:
    case sfk_move_assignment:
      move_p = true;
    case sfk_copy_constructor:
    case sfk_copy_assignment:
      copy_arg_p = true;
      break;

    default:
      gcc_unreachable ();
    }

  expected_trivial = type_has_trivial_fn (ctype, sfk);
  if (trivial_p)
    *trivial_p = expected_trivial;

  /* The TYPE_HAS_COMPLEX_* flags tell us about constraints from base
     class versions and other properties of the type.  But a subobject
     class can be trivially copyable and yet have overload resolution
     choose a template constructor for initialization, depending on
     rvalueness and cv-quals.  And furthermore, a member in a base might
     be trivial but deleted or otherwise not callable.  So we can't exit
     early in C++0x.  The same considerations apply in C++98/03, but
     there the definition of triviality does not consider overload
     resolution, so a constructor can be trivial even if it would otherwise
     call a non-trivial constructor.  */
  if (expected_trivial
      && (!copy_arg_p || cxx_dialect < cxx11))
    {
      if (constexpr_p && sfk == sfk_constructor)
	{
	  bool cx = trivial_default_constructor_is_constexpr (ctype);
	  *constexpr_p = cx;
	  if (diag && !cx && TREE_CODE (ctype) == UNION_TYPE)
	    /* A trivial constructor doesn't have any NSDMI.  */
	    inform (input_location, "defaulted default constructor does "
		    "not initialize any non-static data member");
	}
      if (!diag && cxx_dialect < cxx11)
	return;
    }

  ++cp_unevaluated_operand;
  ++c_inhibit_evaluation_warnings;
  push_deferring_access_checks (dk_no_deferred);

  scope = push_scope (ctype);

  flags = LOOKUP_NORMAL|LOOKUP_SPECULATIVE;
  if (!inherited_base)
    flags |= LOOKUP_DEFAULTED;

  complain = diag ? tf_warning_or_error : tf_none;

  if (const_p)
    quals = TYPE_QUAL_CONST;
  else
    quals = TYPE_UNQUALIFIED;
  argtype = NULL_TREE;

  for (binfo = TYPE_BINFO (ctype), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    {
      tree basetype = BINFO_TYPE (base_binfo);

      if (!assign_p && BINFO_VIRTUAL_P (base_binfo))
	/* We'll handle virtual bases below.  */
	continue;

      if (copy_arg_p)
	argtype = build_stub_type (basetype, quals, move_p);
      else if (basetype == inherited_base)
	argtype = inherited_parms;
      rval = locate_fn_flags (base_binfo, fnname, argtype, flags, complain);
      if (inherited_base)
	argtype = NULL_TREE;

      process_subob_fn (rval, spec_p, trivial_p, deleted_p,
			constexpr_p, diag, basetype);
      if (ctor_p)
	{
	  /* In a constructor we also need to check the subobject
	     destructors for cleanup of partially constructed objects.  */
	  rval = locate_fn_flags (base_binfo, complete_dtor_identifier,
				  NULL_TREE, flags, complain);
	  /* Note that we don't pass down trivial_p; the subobject
	     destructors don't affect triviality of the constructor.  Nor
	     do they affect constexpr-ness (a constant expression doesn't
	     throw) or exception-specification (a throw from one of the
	     dtors would be a double-fault).  */
	  process_subob_fn (rval, NULL, NULL,
			    deleted_p, NULL, false,
			    basetype);
	}

      if (check_vdtor && type_has_virtual_destructor (basetype))
	{
	  rval = locate_fn_flags (ctype, ansi_opname (DELETE_EXPR),
				  ptr_type_node, flags, complain);
	  /* Unlike for base ctor/op=/dtor, for operator delete it's fine
	     to have a null rval (no class-specific op delete).  */
	  if (rval && rval == error_mark_node && deleted_p)
	    *deleted_p = true;
	  check_vdtor = false;
	}

      if (diag && assign_p && move_p
	  && BINFO_VIRTUAL_P (base_binfo)
	  && rval && TREE_CODE (rval) == FUNCTION_DECL
	  && move_fn_p (rval) && !trivial_fn_p (rval)
	  && vbase_has_user_provided_move_assign (basetype))
	warning (OPT_Wvirtual_move_assign,
		 "defaulted move assignment for %qT calls a non-trivial "
		 "move assignment operator for virtual base %qT",
		 ctype, basetype);
    }

  vbases = CLASSTYPE_VBASECLASSES (ctype);
  if (vbases == NULL)
    /* No virtual bases to worry about.  */;
  else if (!assign_p)
    {
      if (constexpr_p)
	*constexpr_p = false;
      FOR_EACH_VEC_ELT (*vbases, i, base_binfo)
	{
	  tree basetype = BINFO_TYPE (base_binfo);
	  if (copy_arg_p)
	    argtype = build_stub_type (basetype, quals, move_p);
	  rval = locate_fn_flags (base_binfo, fnname, argtype, flags, complain);

	  process_subob_fn (rval, spec_p, trivial_p, deleted_p,
			    constexpr_p, diag, basetype);
	  if (ctor_p && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (basetype))
	    {
	      rval = locate_fn_flags (base_binfo, complete_dtor_identifier,
				      NULL_TREE, flags, complain);
	      process_subob_fn (rval, NULL, NULL,
				deleted_p, NULL, false,
				basetype);
	    }
	}
    }

  /* Now handle the non-static data members.  */
  walk_field_subobs (TYPE_FIELDS (ctype), fnname, sfk, quals,
		     copy_arg_p, move_p, assign_p, spec_p, trivial_p,
		     deleted_p, constexpr_p,
		     diag, flags, complain);
  if (ctor_p)
    walk_field_subobs (TYPE_FIELDS (ctype), complete_dtor_identifier,
		       sfk_destructor, TYPE_UNQUALIFIED, false,
		       false, false, NULL, NULL,
		       deleted_p, NULL,
		       false, flags, complain);

  pop_scope (scope);

  pop_deferring_access_checks ();
  --cp_unevaluated_operand;
  --c_inhibit_evaluation_warnings;
}

/* DECL is a deleted function.  If it's implicitly deleted, explain why and
   return true; else return false.  */

bool
maybe_explain_implicit_delete (tree decl)
{
  /* If decl is a clone, get the primary variant.  */
  decl = DECL_ORIGIN (decl);
  gcc_assert (DECL_DELETED_FN (decl));
  if (DECL_DEFAULTED_FN (decl))
    {
      /* Not marked GTY; it doesn't need to be GC'd or written to PCH.  */
      static struct pointer_set_t *explained;

      special_function_kind sfk;
      location_t loc;
      bool informed;
      tree ctype;

      if (!explained)
	explained = pointer_set_create ();
      if (pointer_set_insert (explained, decl))
	return true;

      sfk = special_function_p (decl);
      ctype = DECL_CONTEXT (decl);
      loc = input_location;
      input_location = DECL_SOURCE_LOCATION (decl);

      informed = false;
      if (LAMBDA_TYPE_P (ctype))
	{
	  informed = true;
	  if (sfk == sfk_constructor)
	    inform (DECL_SOURCE_LOCATION (decl),
		    "a lambda closure type has a deleted default constructor");
	  else if (sfk == sfk_copy_assignment)
	    inform (DECL_SOURCE_LOCATION (decl),
		    "a lambda closure type has a deleted copy assignment operator");
	  else
	    informed = false;
	}
      else if (DECL_ARTIFICIAL (decl)
	       && (sfk == sfk_copy_assignment
		   || sfk == sfk_copy_constructor)
	       && (type_has_user_declared_move_constructor (ctype)
		   || type_has_user_declared_move_assign (ctype)))
	{
	  inform (0, "%q+#D is implicitly declared as deleted because %qT "
		 "declares a move constructor or move assignment operator",
		 decl, ctype);
	  informed = true;
	}
      if (!informed)
	{
	  tree parms = FUNCTION_FIRST_USER_PARMTYPE (decl);
	  tree parm_type = TREE_VALUE (parms);
	  bool const_p = CP_TYPE_CONST_P (non_reference (parm_type));
	  tree raises = NULL_TREE;
	  bool deleted_p = false;
	  tree scope = push_scope (ctype);

	  synthesized_method_walk (ctype, sfk, const_p,
				   &raises, NULL, &deleted_p, NULL, false,
				   DECL_INHERITED_CTOR_BASE (decl), parms);
	  if (deleted_p)
	    {
	      inform (0, "%q+#D is implicitly deleted because the default "
		      "definition would be ill-formed:", decl);
	      synthesized_method_walk (ctype, sfk, const_p,
				       NULL, NULL, NULL, NULL, true,
				       DECL_INHERITED_CTOR_BASE (decl), parms);
	    }
	  else if (!comp_except_specs
		   (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (decl)),
		    raises, ce_normal))
	    inform (DECL_SOURCE_LOCATION (decl), "%q#F is implicitly "
		    "deleted because its exception-specification does not "
		    "match the implicit exception-specification %qX",
		    decl, raises);
#ifdef ENABLE_CHECKING
	  else
	    gcc_unreachable ();
#endif

	  pop_scope (scope);
	}

      input_location = loc;
      return true;
    }
  return false;
}

/* DECL is a defaulted function which was declared constexpr.  Explain why
   it can't be constexpr.  */

void
explain_implicit_non_constexpr (tree decl)
{
  tree parm_type = TREE_VALUE (FUNCTION_FIRST_USER_PARMTYPE (decl));
  bool const_p = CP_TYPE_CONST_P (non_reference (parm_type));
  bool dummy;
  synthesized_method_walk (DECL_CLASS_CONTEXT (decl),
			   special_function_p (decl), const_p,
			   NULL, NULL, NULL, &dummy, true,
			   NULL_TREE, NULL_TREE);
}

/* DECL is an instantiation of an inheriting constructor template.  Deduce
   the correct exception-specification and deletedness for this particular
   specialization.  */

void
deduce_inheriting_ctor (tree decl)
{
  gcc_assert (DECL_INHERITED_CTOR_BASE (decl));
  tree spec;
  bool trivial, constexpr_, deleted;
  synthesized_method_walk (DECL_CONTEXT (decl), sfk_inheriting_constructor,
			   false, &spec, &trivial, &deleted, &constexpr_,
			   /*diag*/false,
			   DECL_INHERITED_CTOR_BASE (decl),
			   FUNCTION_FIRST_USER_PARMTYPE (decl));
  DECL_DELETED_FN (decl) = deleted;
  TREE_TYPE (decl) = build_exception_variant (TREE_TYPE (decl), spec);
}

/* Implicitly declare the special function indicated by KIND, as a
   member of TYPE.  For copy constructors and assignment operators,
   CONST_P indicates whether these functions should take a const
   reference argument or a non-const reference.  Returns the
   FUNCTION_DECL for the implicitly declared function.  */

tree
implicitly_declare_fn (special_function_kind kind, tree type,
		       bool const_p, tree inherited_ctor,
		       tree inherited_parms)
{
  tree fn;
  tree parameter_types = void_list_node;
  tree return_type;
  tree fn_type;
  tree raises = empty_except_spec;
  tree rhs_parm_type = NULL_TREE;
  tree this_parm;
  tree name;
  HOST_WIDE_INT saved_processing_template_decl;
  bool deleted_p;
  bool constexpr_p;

  /* Because we create declarations for implicitly declared functions
     lazily, we may be creating the declaration for a member of TYPE
     while in some completely different context.  However, TYPE will
     never be a dependent class (because we never want to do lookups
     for implicitly defined functions in a dependent class).
     Furthermore, we must set PROCESSING_TEMPLATE_DECL to zero here
     because we only create clones for constructors and destructors
     when not in a template.  */
  gcc_assert (!dependent_type_p (type));
  saved_processing_template_decl = processing_template_decl;
  processing_template_decl = 0;

  type = TYPE_MAIN_VARIANT (type);

  if (targetm.cxx.cdtor_returns_this () && !TYPE_FOR_JAVA (type))
    {
      if (kind == sfk_destructor)
	/* See comment in check_special_function_return_type.  */
	return_type = build_pointer_type (void_type_node);
      else
	return_type = build_pointer_type (type);
    }
  else
    return_type = void_type_node;

  switch (kind)
    {
    case sfk_destructor:
      /* Destructor.  */
      name = constructor_name (type);
      break;

    case sfk_constructor:
      /* Default constructor.  */
      name = constructor_name (type);
      break;

    case sfk_copy_constructor:
    case sfk_copy_assignment:
    case sfk_move_constructor:
    case sfk_move_assignment:
    case sfk_inheriting_constructor:
    {
      bool move_p;
      if (kind == sfk_copy_assignment
	  || kind == sfk_move_assignment)
	{
	  return_type = build_reference_type (type);
	  name = ansi_assopname (NOP_EXPR);
	}
      else
	name = constructor_name (type);

      if (kind == sfk_inheriting_constructor)
	parameter_types = inherited_parms;
      else
	{
	  if (const_p)
	    rhs_parm_type = cp_build_qualified_type (type, TYPE_QUAL_CONST);
	  else
	    rhs_parm_type = type;
	  move_p = (kind == sfk_move_assignment
		    || kind == sfk_move_constructor);
	  rhs_parm_type = cp_build_reference_type (rhs_parm_type, move_p);

	  parameter_types = tree_cons (NULL_TREE, rhs_parm_type, parameter_types);
	}
      break;
    }
    default:
      gcc_unreachable ();
    }

  tree inherited_base = (inherited_ctor
			 ? DECL_CONTEXT (inherited_ctor)
			 : NULL_TREE);
  bool trivial_p = false;

  if (inherited_ctor && TREE_CODE (inherited_ctor) == TEMPLATE_DECL)
    {
      /* For an inheriting constructor template, just copy these flags from
	 the inherited constructor template for now.  */
      raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (inherited_ctor));
      deleted_p = DECL_DELETED_FN (DECL_TEMPLATE_RESULT (inherited_ctor));
      constexpr_p
	= DECL_DECLARED_CONSTEXPR_P (DECL_TEMPLATE_RESULT (inherited_ctor));
    }
  else
    synthesized_method_walk (type, kind, const_p, &raises, &trivial_p,
			     &deleted_p, &constexpr_p, false,
			     inherited_base, inherited_parms);
  /* Don't bother marking a deleted constructor as constexpr.  */
  if (deleted_p)
    constexpr_p = false;
  /* A trivial copy/move constructor is also a constexpr constructor.  */
  else if (trivial_p && cxx_dialect >= cxx11
	   && (kind == sfk_copy_constructor
	       || kind == sfk_move_constructor))
    gcc_assert (constexpr_p);

  if (!trivial_p && type_has_trivial_fn (type, kind))
    type_set_nontrivial_flag (type, kind);

  /* Create the function.  */
  fn_type = build_method_type_directly (type, return_type, parameter_types);
  if (raises)
    fn_type = build_exception_variant (fn_type, raises);
  fn = build_lang_decl (FUNCTION_DECL, name, fn_type);
  if (kind != sfk_inheriting_constructor)
    DECL_SOURCE_LOCATION (fn) = DECL_SOURCE_LOCATION (TYPE_NAME (type));
  if (kind == sfk_constructor || kind == sfk_copy_constructor
      || kind == sfk_move_constructor || kind == sfk_inheriting_constructor)
    DECL_CONSTRUCTOR_P (fn) = 1;
  else if (kind == sfk_destructor)
    DECL_DESTRUCTOR_P (fn) = 1;
  else
    {
      DECL_ASSIGNMENT_OPERATOR_P (fn) = 1;
      SET_OVERLOADED_OPERATOR_CODE (fn, NOP_EXPR);
    }
  
  /* If pointers to member functions use the least significant bit to
     indicate whether a function is virtual, ensure a pointer
     to this function will have that bit clear.  */
  if (TARGET_PTRMEMFUNC_VBIT_LOCATION == ptrmemfunc_vbit_in_pfn
      && DECL_ALIGN (fn) < 2 * BITS_PER_UNIT)
    DECL_ALIGN (fn) = 2 * BITS_PER_UNIT;

  /* Create the explicit arguments.  */
  if (rhs_parm_type)
    {
      /* Note that this parameter is *not* marked DECL_ARTIFICIAL; we
	 want its type to be included in the mangled function
	 name.  */
      tree decl = cp_build_parm_decl (NULL_TREE, rhs_parm_type);
      TREE_READONLY (decl) = 1;
      retrofit_lang_decl (decl);
      DECL_PARM_INDEX (decl) = DECL_PARM_LEVEL (decl) = 1;
      DECL_ARGUMENTS (fn) = decl;
    }
  else if (kind == sfk_inheriting_constructor)
    {
      tree *p = &DECL_ARGUMENTS (fn);
      int index = 1;
      for (tree parm = inherited_parms; parm != void_list_node;
	   parm = TREE_CHAIN (parm))
	{
	  *p = cp_build_parm_decl (NULL_TREE, TREE_VALUE (parm));
	  retrofit_lang_decl (*p);
	  DECL_PARM_LEVEL (*p) = 1;
	  DECL_PARM_INDEX (*p) = index++;
	  DECL_CONTEXT (*p) = fn;
	  p = &DECL_CHAIN (*p);
	}
      SET_DECL_INHERITED_CTOR_BASE (fn, inherited_base);
      DECL_NONCONVERTING_P (fn) = DECL_NONCONVERTING_P (inherited_ctor);
      /* A constructor so declared has the same access as the corresponding
	 constructor in X.  */
      TREE_PRIVATE (fn) = TREE_PRIVATE (inherited_ctor);
      TREE_PROTECTED (fn) = TREE_PROTECTED (inherited_ctor);
      /* Copy constexpr from the inherited constructor even if the
	 inheriting constructor doesn't satisfy the requirements.  */
      constexpr_p
	= DECL_DECLARED_CONSTEXPR_P (STRIP_TEMPLATE (inherited_ctor));
    }
  /* Add the "this" parameter.  */
  this_parm = build_this_parm (fn_type, TYPE_UNQUALIFIED);
  DECL_CHAIN (this_parm) = DECL_ARGUMENTS (fn);
  DECL_ARGUMENTS (fn) = this_parm;

  grokclassfn (type, fn, kind == sfk_destructor ? DTOR_FLAG : NO_SPECIAL);
  set_linkage_according_to_type (type, fn);
  rest_of_decl_compilation (fn, toplevel_bindings_p (), at_eof);
  DECL_IN_AGGR_P (fn) = 1;
  DECL_ARTIFICIAL (fn) = 1;
  DECL_DEFAULTED_FN (fn) = 1;
  if (cxx_dialect >= cxx11)
    {
      DECL_DELETED_FN (fn) = deleted_p;
      DECL_DECLARED_CONSTEXPR_P (fn) = constexpr_p;
    }
  DECL_EXTERNAL (fn) = true;
  DECL_NOT_REALLY_EXTERN (fn) = 1;
  DECL_DECLARED_INLINE_P (fn) = 1;
  gcc_assert (!TREE_USED (fn));

  /* Restore PROCESSING_TEMPLATE_DECL.  */
  processing_template_decl = saved_processing_template_decl;

  if (inherited_ctor && TREE_CODE (inherited_ctor) == TEMPLATE_DECL)
    fn = add_inherited_template_parms (fn, inherited_ctor);

  /* Warn about calling a non-trivial move assignment in a virtual base.  */
  if (kind == sfk_move_assignment && !deleted_p && !trivial_p
      && CLASSTYPE_VBASECLASSES (type))
    {
      location_t loc = input_location;
      input_location = DECL_SOURCE_LOCATION (fn);
      synthesized_method_walk (type, kind, const_p,
			       NULL, NULL, NULL, NULL, true,
			       NULL_TREE, NULL_TREE);
      input_location = loc;
    }

  return fn;
}

/* Gives any errors about defaulted functions which need to be deferred
   until the containing class is complete.  */

void
defaulted_late_check (tree fn)
{
  /* Complain about invalid signature for defaulted fn.  */
  tree ctx = DECL_CONTEXT (fn);
  special_function_kind kind = special_function_p (fn);
  bool fn_const_p = (copy_fn_p (fn) == 2);
  tree implicit_fn = implicitly_declare_fn (kind, ctx, fn_const_p,
					    NULL, NULL);
  tree eh_spec = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (implicit_fn));

  if (!same_type_p (TREE_TYPE (TREE_TYPE (fn)),
		    TREE_TYPE (TREE_TYPE (implicit_fn)))
      || !compparms (TYPE_ARG_TYPES (TREE_TYPE (fn)),
		     TYPE_ARG_TYPES (TREE_TYPE (implicit_fn))))
    {
      error ("defaulted declaration %q+D", fn);
      error_at (DECL_SOURCE_LOCATION (fn),
		"does not match expected signature %qD", implicit_fn);
    }

  /* 8.4.2/2: An explicitly-defaulted function (...) may have an explicit
     exception-specification only if it is compatible (15.4) with the 
     exception-specification on the implicit declaration.  If a function
     is explicitly defaulted on its first declaration, (...) it is
     implicitly considered to have the same exception-specification as if
     it had been implicitly declared.  */
  if (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn)))
    {
      maybe_instantiate_noexcept (fn);
      if (!comp_except_specs (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn)),
			      eh_spec, ce_normal))
	{
	  if (DECL_DEFAULTED_IN_CLASS_P (fn))
	    {
	      DECL_DELETED_FN (fn) = true;
	      eh_spec = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn));
	    }
	  else
	    error ("function %q+D defaulted on its redeclaration "
		   "with an exception-specification that differs from "
		   "the implicit declaration %q#D", fn, implicit_fn);
	}
    }
  if (DECL_DEFAULTED_IN_CLASS_P (fn))
    TREE_TYPE (fn) = build_exception_variant (TREE_TYPE (fn), eh_spec);

  if (DECL_DEFAULTED_IN_CLASS_P (fn)
      && DECL_DECLARED_CONSTEXPR_P (implicit_fn))
    {
      /* Hmm...should we do this for out-of-class too? Should it be OK to
	 add constexpr later like inline, rather than requiring
	 declarations to match?  */
      DECL_DECLARED_CONSTEXPR_P (fn) = true;
      if (kind == sfk_constructor)
	TYPE_HAS_CONSTEXPR_CTOR (ctx) = true;
    }

  if (!DECL_DECLARED_CONSTEXPR_P (implicit_fn)
      && DECL_DECLARED_CONSTEXPR_P (fn))
    {
      if (!CLASSTYPE_TEMPLATE_INSTANTIATION (ctx))
	{
	  error ("explicitly defaulted function %q+D cannot be declared "
		 "as constexpr because the implicit declaration is not "
		 "constexpr:", fn);
	  explain_implicit_non_constexpr (fn);
	}
      DECL_DECLARED_CONSTEXPR_P (fn) = false;
    }

  if (DECL_DELETED_FN (implicit_fn))
    DECL_DELETED_FN (fn) = 1;
}

/* Returns true iff FN can be explicitly defaulted, and gives any
   errors if defaulting FN is ill-formed.  */

bool
defaultable_fn_check (tree fn)
{
  special_function_kind kind = sfk_none;

  if (template_parm_scope_p ())
    {
      error ("a template cannot be defaulted");
      return false;
    }

  if (DECL_CONSTRUCTOR_P (fn))
    {
      if (FUNCTION_FIRST_USER_PARMTYPE (fn) == void_list_node)
	kind = sfk_constructor;
      else if (copy_fn_p (fn) > 0
	       && (TREE_CHAIN (FUNCTION_FIRST_USER_PARMTYPE (fn))
		   == void_list_node))
	kind = sfk_copy_constructor;
      else if (move_fn_p (fn))
	kind = sfk_move_constructor;
    }
  else if (DECL_DESTRUCTOR_P (fn))
    kind = sfk_destructor;
  else if (DECL_ASSIGNMENT_OPERATOR_P (fn)
	   && DECL_OVERLOADED_OPERATOR_P (fn) == NOP_EXPR)
    {
      if (copy_fn_p (fn))
	kind = sfk_copy_assignment;
      else if (move_fn_p (fn))
	kind = sfk_move_assignment;
    }

  if (kind == sfk_none)
    {
      error ("%qD cannot be defaulted", fn);
      return false;
    }
  else
    {
      for (tree t = FUNCTION_FIRST_USER_PARMTYPE (fn);
	   t && t != void_list_node; t = TREE_CHAIN (t))
	if (TREE_PURPOSE (t))
	  {
	    error ("defaulted function %q+D with default argument", fn);
	    break;
	  }

      /* Avoid do_warn_unused_parameter warnings.  */
      for (tree p = FUNCTION_FIRST_USER_PARM (fn); p; p = DECL_CHAIN (p))
	if (DECL_NAME (p))
	  TREE_NO_WARNING (p) = 1;

      if (TYPE_BEING_DEFINED (DECL_CONTEXT (fn)))
	/* Defer checking.  */;
      else if (!processing_template_decl)
	defaulted_late_check (fn);

      return true;
    }
}

/* Add an implicit declaration to TYPE for the kind of function
   indicated by SFK.  Return the FUNCTION_DECL for the new implicit
   declaration.  */

tree
lazily_declare_fn (special_function_kind sfk, tree type)
{
  tree fn;
  /* Whether or not the argument has a const reference type.  */
  bool const_p = false;

  switch (sfk)
    {
    case sfk_constructor:
      CLASSTYPE_LAZY_DEFAULT_CTOR (type) = 0;
      break;
    case sfk_copy_constructor:
      const_p = TYPE_HAS_CONST_COPY_CTOR (type);
      CLASSTYPE_LAZY_COPY_CTOR (type) = 0;
      break;
    case sfk_move_constructor:
      CLASSTYPE_LAZY_MOVE_CTOR (type) = 0;
      break;
    case sfk_copy_assignment:
      const_p = TYPE_HAS_CONST_COPY_ASSIGN (type);
      CLASSTYPE_LAZY_COPY_ASSIGN (type) = 0;
      break;
    case sfk_move_assignment:
      CLASSTYPE_LAZY_MOVE_ASSIGN (type) = 0;
      break;
    case sfk_destructor:
      CLASSTYPE_LAZY_DESTRUCTOR (type) = 0;
      break;
    default:
      gcc_unreachable ();
    }

  /* Declare the function.  */
  fn = implicitly_declare_fn (sfk, type, const_p, NULL, NULL);

  /* [class.copy]/8 If the class definition declares a move constructor or
     move assignment operator, the implicitly declared copy constructor is
     defined as deleted.... */
  if ((sfk == sfk_copy_assignment
       || sfk == sfk_copy_constructor)
      && (type_has_user_declared_move_constructor (type)
	  || type_has_user_declared_move_assign (type)))
    DECL_DELETED_FN (fn) = true;

  /* A destructor may be virtual.  */
  if (sfk == sfk_destructor
      || sfk == sfk_move_assignment
      || sfk == sfk_copy_assignment)
    check_for_override (fn, type);
  /* Add it to CLASSTYPE_METHOD_VEC.  */
  add_method (type, fn, NULL_TREE);
  /* Add it to TYPE_METHODS.  */
  if (sfk == sfk_destructor
      && DECL_VIRTUAL_P (fn)
      && abi_version_at_least (2))
    /* The ABI requires that a virtual destructor go at the end of the
       vtable.  */
    TYPE_METHODS (type) = chainon (TYPE_METHODS (type), fn);
  else
    {
      /* G++ 3.2 put the implicit destructor at the *beginning* of the
	 TYPE_METHODS list, which cause the destructor to be emitted
	 in an incorrect location in the vtable.  */
      if (warn_abi && sfk == sfk_destructor && DECL_VIRTUAL_P (fn))
	warning (OPT_Wabi, "vtable layout for class %qT may not be ABI-compliant"
		 "and may change in a future version of GCC due to "
		 "implicit virtual destructor",
		 type);
      DECL_CHAIN (fn) = TYPE_METHODS (type);
      TYPE_METHODS (type) = fn;
    }
  maybe_add_class_template_decl_list (type, fn, /*friend_p=*/0);
  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (fn)
      || DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (fn))
    /* Create appropriate clones.  */
    clone_function_decl (fn, /*update_method_vec=*/true);

  return fn;
}

/* Given a FUNCTION_DECL FN and a chain LIST, skip as many elements of LIST
   as there are artificial parms in FN.  */

tree
skip_artificial_parms_for (const_tree fn, tree list)
{
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
    list = TREE_CHAIN (list);
  else
    return list;

  if (DECL_HAS_IN_CHARGE_PARM_P (fn))
    list = TREE_CHAIN (list);
  if (DECL_HAS_VTT_PARM_P (fn))
    list = TREE_CHAIN (list);
  return list;
}

/* Given a FUNCTION_DECL FN and a chain LIST, return the number of
   artificial parms in FN.  */

int
num_artificial_parms_for (const_tree fn)
{
  int count = 0;

  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
    count++;
  else
    return 0;

  if (DECL_HAS_IN_CHARGE_PARM_P (fn))
    count++;
  if (DECL_HAS_VTT_PARM_P (fn))
    count++;
  return count;
}


#include "gt-cp-method.h"
