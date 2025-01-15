/* Handle the hair of processing (but not expanding) inline functions.
   Also manage function and variable name overloading.
   Copyright (C) 1987-2025 Free Software Foundation, Inc.
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
#include "target.h"
#include "cp-tree.h"
#include "decl.h"
#include "stringpool.h"
#include "cgraph.h"
#include "varasm.h"
#include "toplev.h"
#include "intl.h"
#include "common/common-target.h"

static void do_build_copy_assign (tree);
static void do_build_copy_constructor (tree);
static tree make_alias_for_thunk (tree);

/* Called once to initialize method.cc.  */

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

  d = tree_to_shwi (fixed_offset);

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
  DECL_SAVED_AUTO_RETURN_TYPE (thunk) = NULL;
  /* The thunk itself is not a constructor or destructor, even if
     the thing it is thunking to is.  */
  DECL_CXX_DESTRUCTOR_P (thunk) = 0;
  DECL_CXX_CONSTRUCTOR_P (thunk) = 0;
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
		       fixed_offset, virtual_offset, thunk);

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
  DECL_CONTEXT (alias) = DECL_CONTEXT (target);
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
      DECL_SAVED_AUTO_RETURN_TYPE (alias) = NULL;
      DECL_CXX_DESTRUCTOR_P (alias) = 0;
      DECL_CXX_CONSTRUCTOR_P (alias) = 0;
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
      funcn = cgraph_node::get (function);
      gcc_checking_assert (funcn);
      aliasn = cgraph_node::create_same_body_alias (alias, function);
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

  /* Don't diagnose deprecated or unavailable functions just because they
     have thunks emitted for them.  */
  auto du = make_temp_override (deprecated_state,
                                UNAVAILABLE_DEPRECATED_SUPPRESS);

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
      virtual_value = tree_to_shwi (virtual_offset);
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
      tree fn = function;
      struct symtab_node *symbol;

      if ((symbol = symtab_node::get (function))
	  && symbol->alias)
	{
	  if (symbol->analyzed)
	    fn = symtab_node::get (function)->ultimate_alias_target ()->decl;
	  else
	    fn = symtab_node::get (function)->alias_target;
	}
      resolve_unique_section (fn, 0, flag_function_sections);

      if (DECL_SECTION_NAME (fn) != NULL && DECL_ONE_ONLY (fn))
	{
	  resolve_unique_section (thunk_fndecl, 0, flag_function_sections);

	  /* Output the thunk into the same section as function.  */
	  set_decl_section_name (thunk_fndecl, fn);
	  symtab_node::get (thunk_fndecl)->implicit_section
	    = symtab_node::get (fn)->implicit_section;
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
  funcn = cgraph_node::get (function);
  gcc_checking_assert (funcn);
  thunk_node = funcn->create_thunk (thunk_fndecl, function,
				    this_adjusting, fixed_offset, virtual_value,
				    0, virtual_offset, alias);
  if (DECL_ONE_ONLY (function))
    thunk_node->add_to_same_comdat_group (funcn);

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
    case sfk_virtual_destructor:
      return !TYPE_HAS_NONTRIVIAL_DESTRUCTOR (ctype);
    case sfk_inheriting_constructor:
    case sfk_comparison:
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
  if (TREE_CODE (fn) == TEMPLATE_DECL)
    return false;
  if (!DECL_DEFAULTED_FN (fn))
    return false;

  /* If fn is a clone, get the primary variant.  */
  if (tree prim = DECL_CLONED_FUNCTION (fn))
    fn = prim;
  return type_has_trivial_fn (DECL_CONTEXT (fn), special_function_p (fn));
}

/* PARM is a PARM_DECL for a function which we want to forward to another
   function without changing its value category, a la std::forward.  */

tree
forward_parm (tree parm)
{
  tree exp = convert_from_reference (parm);
  tree type = TREE_TYPE (parm);
  if (DECL_PACK_P (parm))
    type = PACK_EXPANSION_PATTERN (type);
  if (!TYPE_REF_P (type))
    type = cp_build_reference_type (type, /*rval=*/true);
  warning_sentinel w (warn_useless_cast);
  exp = build_static_cast (input_location, type, exp,
			   tf_warning_or_error);
  if (DECL_PACK_P (parm))
    exp = make_pack_expansion (exp);
  return exp;
}

/* Strip all inheriting constructors, if any, to return the original
   constructor from a (possibly indirect) base class.  */

tree
strip_inheriting_ctors (tree dfn)
{
  if (!flag_new_inheriting_ctors)
    return dfn;
  tree fn = dfn;
  while (tree inh = DECL_INHERITED_CTOR (fn))
    fn = OVL_FIRST (inh);

  if (TREE_CODE (fn) == TEMPLATE_DECL
      && TREE_CODE (dfn) == FUNCTION_DECL)
    fn = DECL_TEMPLATE_RESULT (fn);
  return fn;
}

/* Find the binfo for the base subobject of BINFO being initialized by
   inherited constructor FNDECL (a member of a direct base of BINFO).  */

static tree inherited_ctor_binfo (tree, tree);
static tree
inherited_ctor_binfo_1 (tree binfo, tree fndecl)
{
  tree base = DECL_CONTEXT (fndecl);
  tree base_binfo;
  for (int i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    if (BINFO_TYPE (base_binfo) == base)
      return inherited_ctor_binfo (base_binfo, fndecl);

  gcc_unreachable();
}

/* Find the binfo for the base subobject of BINFO being initialized by
   inheriting constructor FNDECL (a member of BINFO), or BINFO if FNDECL is not
   an inheriting constructor.  */

static tree
inherited_ctor_binfo (tree binfo, tree fndecl)
{
  tree inh = DECL_INHERITED_CTOR (fndecl);
  if (!inh)
    return binfo;

  tree results = NULL_TREE;
  for (ovl_iterator iter (inh); iter; ++iter)
    {
      tree one = inherited_ctor_binfo_1 (binfo, *iter);
      if (!results)
	results = one;
      else if (one != results)
	results = tree_cons (NULL_TREE, one, results);
    }
  return results;
}

/* Find the binfo for the base subobject being initialized by inheriting
   constructor FNDECL, or NULL_TREE if FNDECL is not an inheriting
   constructor.  */

tree
inherited_ctor_binfo (tree fndecl)
{
  if (!DECL_INHERITED_CTOR (fndecl))
    return NULL_TREE;
  tree binfo = TYPE_BINFO (DECL_CONTEXT (fndecl));
  return inherited_ctor_binfo (binfo, fndecl);
}


/* True if we should omit all user-declared parameters from a base
   construtor built from complete constructor FN.
   That's when the ctor is inherited from a virtual base.  */

bool
base_ctor_omit_inherited_parms (tree comp_ctor)
{
  gcc_checking_assert (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (comp_ctor));

  if (!flag_new_inheriting_ctors)
    /* We only optimize away the parameters in the new model.  */
    return false;

  if (!CLASSTYPE_VBASECLASSES (DECL_CONTEXT (comp_ctor)))
    return false;

  if (FUNCTION_FIRST_USER_PARMTYPE (comp_ctor) == void_list_node)
    /* No user-declared parameters to omit.  */
    return false;

  for (tree binfo = inherited_ctor_binfo (comp_ctor);
       binfo;
       binfo = BINFO_INHERITANCE_CHAIN (binfo))
    if (BINFO_VIRTUAL_P (binfo))
      return true;

  return false;
}


/* True if we should omit all user-declared parameters from constructor FN,
   because it is a base clone of a ctor inherited from a virtual base.  */

bool
ctor_omit_inherited_parms (tree fn)
{
  gcc_checking_assert (TREE_CODE (fn) == FUNCTION_DECL);

  if (!DECL_BASE_CONSTRUCTOR_P (fn))
    return false;

  return base_ctor_omit_inherited_parms (DECL_CLONED_FUNCTION (fn));
}

/* True iff constructor(s) INH inherited into BINFO initializes INIT_BINFO.
   This can be true for multiple virtual bases as well as one direct
   non-virtual base.  */

static bool
binfo_inherited_from (tree binfo, tree init_binfo, tree inh)
{
  /* inh is an OVERLOAD if we inherited the same constructor along
     multiple paths, check all of them.  */
  for (ovl_iterator iter (inh); iter; ++iter)
    {
      tree fn = *iter;
      tree base = DECL_CONTEXT (fn);
      tree base_binfo = NULL_TREE;
      for (int i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	if (BINFO_TYPE (base_binfo) == base)
	  break;
      if (base_binfo == init_binfo
	  || (flag_new_inheriting_ctors
	      && binfo_inherited_from (base_binfo, init_binfo,
				       DECL_INHERITED_CTOR (fn))))
	return true;
    }
  return false;
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
      if (!binfo_inherited_from (TYPE_BINFO (current_class_type), binfo, inh))
	return member_init_list;

      tree *p = &init;
      init = NULL_TREE;
      for (; parm; parm = DECL_CHAIN (parm))
	{
	  tree exp = forward_parm (parm);
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
  tree inh = DECL_INHERITED_CTOR (fndecl);

  if (!inh)
    parm = convert_from_reference (parm);

  if (trivial)
    {
      if (is_empty_class (current_class_type))
	/* Don't copy the padding byte; it might not have been allocated
	   if *this is a base subobject.  */;
      else if (tree_int_cst_equal (TYPE_SIZE (current_class_type),
				   CLASSTYPE_SIZE (current_class_type)))
	{
	  tree t = cp_build_init_expr (current_class_ref, parm);
	  finish_expr_stmt (t);
	}
      else
	{
	  /* We must only copy the non-tail padding parts.  */
	  tree base_size = CLASSTYPE_SIZE_UNIT (current_class_type);
	  base_size = size_binop (MINUS_EXPR, base_size, size_int (1));
	  tree array_type = build_array_type (unsigned_char_type_node,
					      build_index_type (base_size));
	  tree alias_set = build_int_cst (TREE_TYPE (current_class_ptr), 0);
	  tree lhs = build2 (MEM_REF, array_type,
			     current_class_ptr, alias_set);
	  tree rhs = build2 (MEM_REF, array_type,
			     TREE_OPERAND (parm, 0), alias_set);
	  tree t = cp_build_init_expr (lhs, rhs);
	  finish_expr_stmt (t);
	}
    }
  else
    {
      tree member_init_list = NULL_TREE;
      int i;
      tree binfo, base_binfo;
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

      if (!inh)
	{
	  int cvquals = cp_type_quals (TREE_TYPE (parm));

	  for (tree fields = TYPE_FIELDS (current_class_type);
	       fields; fields = DECL_CHAIN (fields))
	    {
	      tree field = fields;
	      tree expr_type;

	      if (TREE_CODE (field) != FIELD_DECL)
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
	      if (!TYPE_REF_P (expr_type))
		{
		  int quals = cvquals;

		  if (DECL_MUTABLE_P (field))
		    quals &= ~TYPE_QUAL_CONST;
		  quals |= cp_type_quals (expr_type);
		  expr_type = cp_build_qualified_type (expr_type, quals);
		}

	      tree init = build3 (COMPONENT_REF, expr_type, parm, field, NULL_TREE);
	      if (move_p && !TYPE_REF_P (expr_type)
		  /* 'move' breaks bit-fields, and has no effect for scalars.  */
		  && !scalarish_type_p (expr_type))
		init = move (init);
	      init = build_tree_list (NULL_TREE, init);

	      member_init_list = tree_cons (field, init, member_init_list);
	    }
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

  /* If we are building a defaulted xobj copy/move assignment operator then
     current_class_ref will not have been set up.
     Kind of an icky hack, but what can ya do?  */
  tree const class_ref = DECL_XOBJ_MEMBER_FUNCTION_P (fndecl)
    ? cp_build_fold_indirect_ref (DECL_ARGUMENTS (fndecl)) : current_class_ref;

  if (trivial
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (trivial)
    {
      tree t = build2 (MODIFY_EXPR, void_type_node, class_ref, parm);
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

	  /* We must convert PARM directly to the base class
	     explicitly since the base class may be ambiguous.  */
	  converted_parm = build_base_path (PLUS_EXPR, parm, base_binfo, 1,
					    tf_warning_or_error);
	  if (move_p)
	    converted_parm = move (converted_parm);
	  /* Call the base class assignment operator.  */
	  releasing_vec parmvec (make_tree_vector_single (converted_parm));
	  finish_expr_stmt
	    (build_special_member_call (class_ref,
					assign_op_identifier,
					&parmvec,
					base_binfo,
					flags,
                                        tf_warning_or_error));
	}

      /* Assign to each of the non-static data members.  */
      for (fields = TYPE_FIELDS (current_class_type);
	   fields;
	   fields = DECL_CHAIN (fields))
	{
	  tree comp = class_ref;
	  tree init = parm;
	  tree field = fields;
	  tree expr_type;
	  int quals;

	  if (TREE_CODE (field) != FIELD_DECL || DECL_ARTIFICIAL (field))
	    continue;

	  expr_type = TREE_TYPE (field);

	  if (CP_TYPE_CONST_P (expr_type))
	    {
	      error ("non-static const member %q#D, cannot use default "
		     "assignment operator", field);
	      continue;
	    }
	  else if (TYPE_REF_P (expr_type))
	    {
	      error ("non-static reference member %q#D, cannot use "
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
	  if (move_p && !TYPE_REF_P (expr_type)
	      /* 'move' breaks bit-fields, and has no effect for scalars.  */
	      && !scalarish_type_p (expr_type))
	    init = move (init);

	  if (DECL_NAME (field))
	    init = cp_build_modify_expr (input_location, comp, NOP_EXPR, init,
					 tf_warning_or_error);
	  else
	    init = build2 (MODIFY_EXPR, TREE_TYPE (comp), comp, init);
	  finish_expr_stmt (init);
	}
    }
  finish_return_stmt (class_ref);
  finish_compound_stmt (compound_stmt);
}

/* C++20 <compare> comparison category types.  */

enum comp_cat_tag
{
  cc_partial_ordering,
  cc_weak_ordering,
  cc_strong_ordering,
  cc_last
};

/* Names of the comparison categories and their value members, to be indexed by
   comp_cat_tag enumerators.  genericize_spaceship below relies on the ordering
   of the members.  */

struct comp_cat_info_t
{
  const char *name;
  const char *members[4];
};
static const comp_cat_info_t comp_cat_info[cc_last]
= {
   { "partial_ordering", { "equivalent", "greater", "less", "unordered" } },
   { "weak_ordering", { "equivalent", "greater", "less" } },
   { "strong_ordering", { "equal", "greater", "less" } }
};

/* A cache of the category types to speed repeated lookups.  */

static GTY((deletable)) tree comp_cat_cache[cc_last];

/* Look up one of the result variables in the comparison category type.  */

static tree
lookup_comparison_result (tree type, const char *name_str,
			  tsubst_flags_t complain = tf_warning_or_error)
{
  tree name = get_identifier (name_str);
  tree decl = lookup_qualified_name (type, name);
  if (TREE_CODE (decl) != VAR_DECL)
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  if (decl == error_mark_node || TREE_CODE (decl) == TREE_LIST)
	    qualified_name_lookup_error (type, name, decl, input_location);
	  else
	    error ("%qD is not a static data member", decl);
	  inform (input_location, "determining value of %qs", "operator<=>");
	}
      return error_mark_node;
    }
  return decl;
}

/* Look up a <compare> comparison category type in std.  */

static tree
lookup_comparison_category (comp_cat_tag tag,
			    tsubst_flags_t complain = tf_warning_or_error)
{
  if (tree cached = comp_cat_cache[tag])
    return cached;

  tree name = get_identifier (comp_cat_info[tag].name);
  tree decl = lookup_qualified_name (std_node, name);
  if (TREE_CODE (decl) != TYPE_DECL)
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  if (decl == error_mark_node || TREE_CODE (decl) == TREE_LIST)
	    qualified_name_lookup_error (std_node, name, decl, input_location);
	  else
	    error ("%qD is not a type", decl);
	  inform (input_location, "forming type of %qs", "operator<=>");
	}
      return error_mark_node;
    }
  /* Also make sure we can look up the value members now, since we won't
     really use them until genericize time.  */
  tree type = TREE_TYPE (decl);
  for (int i = 0; i < 4; ++i)
    {
      const char *p = comp_cat_info[tag].members[i];
      if (!p) break;
      if (lookup_comparison_result (type, p, complain)
	  == error_mark_node)
	return error_mark_node;
    }
  return comp_cat_cache[tag] = type;
}

/* Wrapper that takes the tag rather than the type.  */

static tree
lookup_comparison_result (comp_cat_tag tag, const char *name_str,
			  tsubst_flags_t complain = tf_warning_or_error)
{
  tree type = lookup_comparison_category (tag, complain);
  return lookup_comparison_result (type, name_str, complain);
}

/* Wrapper that takes the index into the members array instead of the name.  */

static tree
lookup_comparison_result (comp_cat_tag tag, tree type, int idx)
{
  const char *name_str = comp_cat_info[tag].members[idx];
  if (!name_str)
    return NULL_TREE;
  return lookup_comparison_result (type, name_str);
}

/* Does TYPE correspond to TAG?  */

static bool
is_cat (tree type, comp_cat_tag tag)
{
  tree name = TYPE_LINKAGE_IDENTIFIER (type);
  return id_equal (name, comp_cat_info[tag].name);
}

/* Return the comp_cat_tag for TYPE.  */

static comp_cat_tag
cat_tag_for (tree type)
{
  if (!CLASS_TYPE_P (type) || !decl_in_std_namespace_p (TYPE_MAIN_DECL (type)))
    return cc_last;
  for (int i = 0; i < cc_last; ++i)
    {
      comp_cat_tag tag = (comp_cat_tag)i;
      if (is_cat (type, tag))
	return tag;
    }
  return cc_last;
}

/* Return the comparison category tag of a <=> expression with non-class type
   OPTYPE.  */

static comp_cat_tag
spaceship_comp_cat (tree optype)
{
  if (INTEGRAL_OR_ENUMERATION_TYPE_P (optype) || TYPE_PTROBV_P (optype))
    return cc_strong_ordering;
  else if (SCALAR_FLOAT_TYPE_P (optype))
    return cc_partial_ordering;

  /* ??? should vector <=> produce a vector of one of the above?  */
  gcc_unreachable ();
}

/* Return the comparison category type of a <=> expression with non-class type
   OPTYPE.  */

tree
spaceship_type (tree optype, tsubst_flags_t complain)
{
  comp_cat_tag tag = spaceship_comp_cat (optype);
  return lookup_comparison_category (tag, complain);
}

/* Turn <=> with type TYPE and operands OP0 and OP1 into GENERIC.
   This is also used by build_comparison_op for fallback to op< and op==
   in a defaulted op<=>.  */

tree
genericize_spaceship (location_t loc, tree type, tree op0, tree op1)
{
  /* ??? maybe optimize based on knowledge of representation? */
  comp_cat_tag tag = cat_tag_for (type);

  if (tag == cc_last && is_auto (type))
    {
      /* build_comparison_op is checking to see if we want to suggest changing
	 the op<=> return type from auto to a specific comparison category; any
	 category will do for now.  */
      tag = cc_strong_ordering;
      type = lookup_comparison_category (tag, tf_none);
      if (type == error_mark_node)
	return error_mark_node;
    }
  else if (tag == cc_last)
    return error_mark_node;

  tree r;
  bool scalar = SCALAR_TYPE_P (TREE_TYPE (op0));
  if (scalar)
    {
      op0 = save_expr (op0);
      op1 = save_expr (op1);
    }

  tree gt = lookup_comparison_result (tag, type, 1);

  int flags = LOOKUP_NORMAL;
  tsubst_flags_t complain = tf_none;
  tree comp;

  if (tag == cc_partial_ordering)
    {
      /* op0 == op1 ? equivalent : op0 < op1 ? less :
	 op1 < op0 ? greater : unordered */
      tree uo = lookup_comparison_result (tag, type, 3);
      if (scalar)
	{
	  /* For scalars use the low level operations; using build_new_op causes
	     trouble with constexpr eval in the middle of genericize (100367).  */
	  comp = fold_build2 (LT_EXPR, boolean_type_node, op1, op0);
	  r = fold_build3 (COND_EXPR, type, comp, gt, uo);
	}
      else
	{
	  comp = build_new_op (loc, LT_EXPR, flags, op1, op0, complain);
	  r = build_conditional_expr (loc, comp, gt, uo, complain);
	}
    }
  else
    /* op0 == op1 ? equal : op0 < op1 ? less : greater */
    r = gt;

  tree lt = lookup_comparison_result (tag, type, 2);
  if (scalar)
    {
      comp = fold_build2 (LT_EXPR, boolean_type_node, op0, op1);
      r = fold_build3 (COND_EXPR, type, comp, lt, r);
    }
  else
    {
      comp = build_new_op (loc, LT_EXPR, flags, op0, op1, complain);
      r = build_conditional_expr (loc, comp, lt, r, complain);
    }

  tree eq = lookup_comparison_result (tag, type, 0);
  if (scalar)
    {
      comp = fold_build2 (EQ_EXPR, boolean_type_node, op0, op1);
      r = fold_build3 (COND_EXPR, type, comp, eq, r);
    }
  else
    {
      comp = build_new_op (loc, EQ_EXPR, flags, op0, op1, complain);
      r = build_conditional_expr (loc, comp, eq, r, complain);
    }

  return r;
}

/* Check that the signature of a defaulted comparison operator is
   well-formed.  */

static bool
early_check_defaulted_comparison (tree fn)
{
  location_t loc = DECL_SOURCE_LOCATION (fn);
  tree ctx;
  if (DECL_CLASS_SCOPE_P (fn))
    ctx = DECL_CONTEXT (fn);
  else
    ctx = DECL_FRIEND_CONTEXT (fn);
  bool ok = true;

  if (cxx_dialect < cxx20)
    {
      error_at (loc, "defaulted %qD only available with %<-std=c++20%> or "
		     "%<-std=gnu++20%>", fn);
      return false;
    }

  if (!DECL_OVERLOADED_OPERATOR_IS (fn, SPACESHIP_EXPR)
      && !same_type_p (TREE_TYPE (TREE_TYPE (fn)), boolean_type_node))
    {
      diagnostic_t kind = DK_UNSPECIFIED;
      int opt = 0;
      if (is_auto (TREE_TYPE (fn)))
	kind = DK_PEDWARN;
      else
	kind = DK_ERROR;
      emit_diagnostic (kind, loc, opt,
		       "defaulted %qD must return %<bool%>", fn);
      if (kind == DK_ERROR)
	ok = false;
    }

  bool mem = DECL_IOBJ_MEMBER_FUNCTION_P (fn);
  if (mem && type_memfn_quals (TREE_TYPE (fn)) != TYPE_QUAL_CONST)
    {
      error_at (loc, "defaulted %qD must be %<const%>", fn);
      ok = false;
    }
  if (mem && type_memfn_rqual (TREE_TYPE (fn)) == REF_QUAL_RVALUE)
    {
      error_at (loc, "defaulted %qD must not have %<&&%> ref-qualifier", fn);
      ok = false;
    }
  tree parmnode = FUNCTION_FIRST_USER_PARMTYPE (fn);
  bool saw_byval = false;
  bool saw_byref = mem;
  bool saw_bad = false;
  for (; parmnode != void_list_node; parmnode = TREE_CHAIN (parmnode))
    {
      tree parmtype = TREE_VALUE (parmnode);
      if (CLASS_TYPE_P (parmtype))
	saw_byval = true;
      else if (TREE_CODE (parmtype) == REFERENCE_TYPE
	       && !TYPE_REF_IS_RVALUE (parmtype)
	       && TYPE_QUALS (TREE_TYPE (parmtype)) == TYPE_QUAL_CONST)
	{
	  saw_byref = true;
	  parmtype = TREE_TYPE (parmtype);
	}
      else
	saw_bad = true;

      if (!saw_bad && !ctx)
	{
	  /* Defaulted outside the class body.  */
	  ctx = TYPE_MAIN_VARIANT (parmtype);
	  if (!is_friend (ctx, fn))
	    {
	      auto_diagnostic_group d;
	      error_at (loc, "defaulted %qD is not a friend of %qT", fn, ctx);
	      inform (location_of (ctx), "declared here");
	      ok = false;
	    }
	}
      else if (!same_type_ignoring_top_level_qualifiers_p (parmtype, ctx))
	saw_bad = true;
    }

  if (saw_bad || (saw_byval && saw_byref))
    {
      if (DECL_IOBJ_MEMBER_FUNCTION_P (fn))
	error_at (loc, "defaulted member %qD must have parameter type "
		  "%<const %T&%>", fn, ctx);
      else if (saw_bad)
	error_at (loc, "defaulted %qD must have parameters of either type "
		  "%<const %T&%> or %qT", fn, ctx, ctx);
      else
	error_at (loc, "defaulted %qD must have parameters of either type "
		  "%<const %T&%> or %qT, not both", fn, ctx, ctx);
      ok = false;
    }

  /* We still need to deduce deleted/constexpr/noexcept and maybe return. */
  DECL_MAYBE_DELETED (fn) = ok;

  return ok;
}

/* Subroutine of build_comparison_op.  Given the vec of memberwise
   comparisons COMPS, calculate the overall comparison category for
   operator<=>.  */

static tree
common_comparison_type (vec<tree> &comps)
{
  tree seen[cc_last] = {};

  for (unsigned i = 0; i < comps.length(); ++i)
    {
      tree comp = comps[i];
      if (TREE_CODE (comp) == TREE_LIST)
	comp = TREE_VALUE (comp);
      tree ctype = TREE_TYPE (comp);
      comp_cat_tag tag = cat_tag_for (ctype);
      /* build_comparison_op already checked this.  */
      gcc_checking_assert (tag < cc_last);
      seen[tag] = ctype;
    }

  /* Otherwise, if at least one T i is std::partial_ordering, U is
     std::partial_ordering.  */
  if (tree t = seen[cc_partial_ordering]) return t;

  /* Otherwise, if at least one T i is std::weak_ordering, U is
     std::weak_ordering.  */
  if (tree t = seen[cc_weak_ordering]) return t;

  /* Otherwise, U is std::strong_ordering.  */
  if (tree t = seen[cc_strong_ordering]) return t;
  return lookup_comparison_category (cc_strong_ordering);
}

/* Data structure for build_comparison_op.  */

struct comp_info
{
  tree fndecl;
  location_t loc;
  tsubst_flags_t complain;
  tree_code code;
  comp_cat_tag retcat;
  bool first_time;
  bool constexp;
  bool was_constexp;
  bool noex;

  comp_info (tree fndecl, tsubst_flags_t complain)
    : fndecl (fndecl), complain (complain)
  {
    loc = DECL_SOURCE_LOCATION (fndecl);

    first_time = DECL_MAYBE_DELETED (fndecl);
    DECL_MAYBE_DELETED (fndecl) = false;

    /* Do we want to try to set constexpr?  */
    was_constexp = DECL_DECLARED_CONSTEXPR_P (fndecl);
    constexp = first_time;
    if (constexp)
      /* Set this for var_in_constexpr_fn.  */
      DECL_DECLARED_CONSTEXPR_P (fndecl) = true;

    /* Do we want to try to set noexcept?  */
    noex = first_time;
    if (noex)
      {
	tree raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fndecl));
	if (raises && !UNEVALUATED_NOEXCEPT_SPEC_P (raises))
	  /* There was an explicit exception-specification.  */
	  noex = false;
      }
  }

  /* EXPR is an expression built as part of the function body.
     Adjust the properties appropriately.  */
  void check (tree expr)
  {
    if (expr == error_mark_node)
      DECL_DELETED_FN (fndecl) = true;
    if ((constexp || was_constexp)
	&& !potential_rvalue_constant_expression (expr))
      {
	if (was_constexp)
	  require_potential_rvalue_constant_expression_fncheck (expr);
	else
	  constexp = false;
      }
    if (noex && !expr_noexcept_p (expr, tf_none))
      noex = false;
  }

  ~comp_info ()
  {
    if (first_time)
      {
	DECL_DECLARED_CONSTEXPR_P (fndecl) = constexp || was_constexp;
	tree raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fndecl));
	if (!raises || UNEVALUATED_NOEXCEPT_SPEC_P (raises))
	  {
	    raises = noex ? noexcept_true_spec : noexcept_false_spec;
	    TREE_TYPE (fndecl) = build_exception_variant (TREE_TYPE (fndecl),
							  raises);
	  }
      }
  }
};

/* Subroutine of build_comparison_op, to compare a single subobject.  */

static tree
do_one_comp (location_t loc, const comp_info &info, tree sub, tree lhs, tree rhs)
{
  const tree_code code = info.code;
  const tree fndecl = info.fndecl;
  const comp_cat_tag retcat = info.retcat;
  const tsubst_flags_t complain = info.complain;

  tree overload = NULL_TREE;
  int flags = LOOKUP_NORMAL | LOOKUP_NONVIRTUAL | LOOKUP_DEFAULTED;
  /* If we have an explicit comparison category return type we can fall back
     to </=, so don't give an error yet if <=> lookup fails.  */
  bool tentative = retcat != cc_last;
  tree comp = build_new_op (loc, code, flags, lhs, rhs,
			    NULL_TREE, NULL_TREE, &overload,
			    tentative ? tf_none : complain);

  if (code != SPACESHIP_EXPR)
    return comp;

  tree rettype = TREE_TYPE (TREE_TYPE (fndecl));

  if (comp == error_mark_node)
    {
      if (overload == NULL_TREE && (tentative || complain))
	{
	  /* No viable <=>, try using op< and op==.  */
	  tree lteq = genericize_spaceship (loc, rettype, lhs, rhs);
	  if (lteq != error_mark_node)
	    {
	      /* We found usable < and ==.  */
	      if (retcat != cc_last)
		/* Return type is a comparison category, use them.  */
		comp = lteq;
	      else if (complain & tf_error)
		/* Return type is auto, suggest changing it.  */
		inform (info.loc, "changing the return type from %qs "
			"to a comparison category type will allow the "
			"comparison to use %qs and %qs", "auto",
			"operator<", "operator==");
	    }
	  else if (tentative && complain)
	    /* No usable < and ==, give an error for op<=>.  */
	    build_new_op (loc, code, flags, lhs, rhs, complain);
	}
      if (comp == error_mark_node)
	return error_mark_node;
    }

  if (FNDECL_USED_AUTO (fndecl)
      && cat_tag_for (TREE_TYPE (comp)) == cc_last)
    {
      /* The operator function is defined as deleted if ... Ri is not a
	 comparison category type.  */
      if (complain & tf_error)
	inform (loc,
		"three-way comparison of %qD has type %qT, not a "
		"comparison category type", sub, TREE_TYPE (comp));
      return error_mark_node;
    }
  else if (!FNDECL_USED_AUTO (fndecl)
	   && !can_convert (rettype, TREE_TYPE (comp), complain))
    {
      if (complain & tf_error)
	error_at (loc,
		  "three-way comparison of %qD has type %qT, which "
		  "does not convert to %qT",
		  sub, TREE_TYPE (comp), rettype);
      return error_mark_node;
    }

  return comp;
}

/* Build up the definition of a defaulted comparison operator.  Unlike other
   defaulted functions that use synthesized_method_walk to determine whether
   the function is e.g. deleted, for comparisons we use the same code.  We try
   to use synthesize_method at the earliest opportunity and bail out if the
   function ends up being deleted.  */

void
build_comparison_op (tree fndecl, bool defining, tsubst_flags_t complain)
{
  comp_info info (fndecl, complain);

  if (!defining && !(complain & tf_error) && !DECL_MAYBE_DELETED (fndecl))
    return;

  int flags = LOOKUP_NORMAL;
  const ovl_op_info_t *op = IDENTIFIER_OVL_OP_INFO (DECL_NAME (fndecl));
  tree_code code = info.code = op->tree_code;

  tree lhs = DECL_ARGUMENTS (fndecl);
  tree rhs = DECL_CHAIN (lhs);
  if (is_this_parameter (lhs))
    lhs = cp_build_fold_indirect_ref (lhs);
  else
    lhs = convert_from_reference (lhs);
  rhs = convert_from_reference (rhs);
  tree ctype = TYPE_MAIN_VARIANT (TREE_TYPE (lhs));
  gcc_assert (!defining || COMPLETE_TYPE_P (ctype));

  iloc_sentinel ils (info.loc);

  /* A defaulted comparison operator function for class C is defined as
     deleted if ... C has variant members.  */
  if (TREE_CODE (ctype) == UNION_TYPE
      && next_aggregate_field (TYPE_FIELDS (ctype)))
    {
      if (complain & tf_error)
	inform (info.loc, "cannot default compare union %qT", ctype);
      DECL_DELETED_FN (fndecl) = true;
      return;
    }

  tree compound_stmt = NULL_TREE;
  if (defining)
    compound_stmt = begin_compound_stmt (0);
  else
    ++cp_unevaluated_operand;

  tree rettype = TREE_TYPE (TREE_TYPE (fndecl));
  if (code != SPACESHIP_EXPR && is_auto (rettype))
    {
      rettype = boolean_type_node;
      apply_deduced_return_type (fndecl, rettype);
    }

  if (code == EQ_EXPR || code == SPACESHIP_EXPR)
    {
      comp_cat_tag &retcat = (info.retcat = cc_last);
      if (code == SPACESHIP_EXPR && !FNDECL_USED_AUTO (fndecl))
	retcat = cat_tag_for (rettype);

      bool bad = false;
      auto_vec<tree> comps;

      /* Compare the base subobjects.  We handle them this way, rather than in
	 the field loop below, because maybe_instantiate_noexcept might bring
	 us here before we've built the base fields.  */
      for (tree base_binfo : BINFO_BASE_BINFOS (TYPE_BINFO (ctype)))
	{
	  tree lhs_base
	    = build_base_path (PLUS_EXPR, lhs, base_binfo, 0, complain);
	  tree rhs_base
	    = build_base_path (PLUS_EXPR, rhs, base_binfo, 0, complain);

	  location_t loc = DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (ctype));
	  tree comp = do_one_comp (loc, info, BINFO_TYPE (base_binfo),
				   lhs_base, rhs_base);
	  if (comp == error_mark_node)
	    {
	      bad = true;
	      continue;
	    }

	  comps.safe_push (comp);
	}

      /* Now compare the field subobjects.  */
      for (tree field = next_aggregate_field (TYPE_FIELDS (ctype));
	   field;
	   field = next_aggregate_field (DECL_CHAIN (field)))
	{
	  if (DECL_VIRTUAL_P (field) || DECL_FIELD_IS_BASE (field))
	    /* We ignore the vptr, and we already handled bases.  */
	    continue;

	  tree expr_type = TREE_TYPE (field);

	  location_t field_loc = DECL_SOURCE_LOCATION (field);

	  /* A defaulted comparison operator function for class C is defined as
	     deleted if any non-static data member of C is of reference type or
	     C has variant members.  */
	  if (TREE_CODE (expr_type) == REFERENCE_TYPE)
	    {
	      if (complain & tf_error)
		inform (field_loc, "cannot default compare "
			"reference member %qD", field);
	      bad = true;
	      continue;
	    }
	  else if (ANON_UNION_TYPE_P (expr_type)
		   && next_aggregate_field (TYPE_FIELDS (expr_type)))
	    {
	      if (complain & tf_error)
		inform (field_loc, "cannot default compare "
			"anonymous union member");
	      bad = true;
	      continue;
	    }

	  tree lhs_mem = build3_loc (field_loc, COMPONENT_REF, expr_type, lhs,
				     field, NULL_TREE);
	  tree rhs_mem = build3_loc (field_loc, COMPONENT_REF, expr_type, rhs,
				     field, NULL_TREE);
	  tree loop_indexes = NULL_TREE;
	  while (TREE_CODE (expr_type) == ARRAY_TYPE)
	    {
	      /* Flexible array member.  */
	      if (TYPE_DOMAIN (expr_type) == NULL_TREE
		  || TYPE_MAX_VALUE (TYPE_DOMAIN (expr_type)) == NULL_TREE)
		{
		  if (complain & tf_error)
		    inform (field_loc, "cannot default compare "
				       "flexible array member");
		  bad = true;
		  break;
		}
	      tree maxval = TYPE_MAX_VALUE (TYPE_DOMAIN (expr_type));
	      /* [0] array.  No subobjects to compare, just skip it.  */
	      if (integer_all_onesp (maxval))
		break;
	      tree idx;
	      /* [1] array, no loop needed, just add [0] ARRAY_REF.
		 Similarly if !defining.  */
	      if (integer_zerop (maxval) || !defining)
		idx = size_zero_node;
	      /* Some other array, will need runtime loop.  */
	      else
		{
		  idx = force_target_expr (sizetype, maxval, complain);
		  loop_indexes = tree_cons (idx, NULL_TREE, loop_indexes);
		}
	      expr_type = TREE_TYPE (expr_type);
	      lhs_mem = build4_loc (field_loc, ARRAY_REF, expr_type, lhs_mem,
				    idx, NULL_TREE, NULL_TREE);
	      rhs_mem = build4_loc (field_loc, ARRAY_REF, expr_type, rhs_mem,
				    idx, NULL_TREE, NULL_TREE);
	    }
	  if (TREE_CODE (expr_type) == ARRAY_TYPE)
	    continue;

	  tree comp = do_one_comp (field_loc, info, field, lhs_mem, rhs_mem);
	  if (comp == error_mark_node)
	    {
	      bad = true;
	      continue;
	    }

	  /* Most of the time, comp is the expression that should be evaluated
	     to compare the two members.  If the expression needs to be
	     evaluated more than once in a loop, it will be a TREE_LIST
	     instead, whose TREE_VALUE is the expression for one array element,
	     TREE_PURPOSE is innermost iterator temporary and if the array
	     is multidimensional, TREE_CHAIN will contain another TREE_LIST
	     with second innermost iterator in its TREE_PURPOSE and so on.  */
	  if (loop_indexes)
	    {
	      TREE_VALUE (loop_indexes) = comp;
	      comp = loop_indexes;
	    }
	  comps.safe_push (comp);
	}
      if (code == SPACESHIP_EXPR && is_auto (rettype))
	{
	  rettype = common_comparison_type (comps);
	  apply_deduced_return_type (fndecl, rettype);
	}
      tree retvaleq;
      if (code == EQ_EXPR)
	retvaleq = boolean_true_node;
      else
	{
	  tree seql = lookup_comparison_result (cc_strong_ordering,
						"equal", complain);
	  retvaleq = build_static_cast (input_location, rettype, seql,
					complain);
	  if (retvaleq == error_mark_node)
	    bad = true;
	}
      if (bad)
	{
	  DECL_DELETED_FN (fndecl) = true;
	  goto out;
	}
      for (unsigned i = 0; i < comps.length(); ++i)
	{
	  tree comp = comps[i];
	  tree eq, retval = NULL_TREE, if_ = NULL_TREE;
	  tree loop_indexes = NULL_TREE;
	  if (defining)
	    {
	      if (TREE_CODE (comp) == TREE_LIST)
		{
		  loop_indexes = comp;
		  comp = TREE_VALUE (comp);
		  loop_indexes = nreverse (loop_indexes);
		  for (tree loop_index = loop_indexes; loop_index;
		       loop_index = TREE_CHAIN (loop_index))
		    {
		      tree for_stmt = begin_for_stmt (NULL_TREE, NULL_TREE);
		      tree idx = TREE_PURPOSE (loop_index);
		      tree maxval = TARGET_EXPR_INITIAL (idx);
		      TARGET_EXPR_INITIAL (idx) = size_zero_node;
		      add_stmt (idx);
		      finish_init_stmt (for_stmt);
		      finish_for_cond (build2 (LE_EXPR, boolean_type_node, idx,
					       maxval), for_stmt, false, 0,
					       false);
		      finish_for_expr (cp_build_unary_op (PREINCREMENT_EXPR,
							  TARGET_EXPR_SLOT (idx),
							  false, complain),
							  for_stmt);
		      /* Store in TREE_VALUE the for_stmt tree, so that we can
			 later on call finish_for_stmt on it (in the reverse
			 order).  */
		      TREE_VALUE (loop_index) = for_stmt;
		    }
		  loop_indexes = nreverse (loop_indexes);
		}
	      if_ = begin_if_stmt ();
	    }
	  /* Spaceship is specified to use !=, but for the comparison category
	     types, != is equivalent to !(==), so let's use == directly.  */
	  if (code == EQ_EXPR)
	    {
	      /* if (x==y); else return false; */
	      eq = comp;
	      retval = boolean_false_node;
	    }
	  else
	    {
	      /* if (auto v = x<=>y, v == 0); else return v; */
	      if (TREE_CODE (comp) == SPACESHIP_EXPR)
		TREE_TYPE (comp) = rettype;
	      else
		comp = build_static_cast (input_location, rettype, comp,
					  complain);
	      info.check (comp);
	      if (defining)
		{
		  tree var = create_temporary_var (rettype);
		  DECL_NAME (var) = get_identifier ("retval");
		  pushdecl (var);
		  cp_finish_decl (var, comp, false, NULL_TREE, flags);
		  comp = retval = var;
		}
	      eq = build_new_op (info.loc, EQ_EXPR, flags, comp,
				 integer_zero_node, NULL_TREE, NULL_TREE,
				 NULL, complain);
	    }
	  tree ceq = contextual_conv_bool (eq, complain);
	  info.check (ceq);
	  if (defining)
	    {
	      finish_if_stmt_cond (ceq, if_);
	      finish_then_clause (if_);
	      begin_else_clause (if_);
	      finish_return_stmt (retval);
	      finish_else_clause (if_);
	      finish_if_stmt (if_);
	      for (tree loop_index = loop_indexes; loop_index;
		   loop_index = TREE_CHAIN (loop_index))
		finish_for_stmt (TREE_VALUE (loop_index));
	    }
	}
      if (defining)
	finish_return_stmt (retvaleq);
    }
  else if (code == NE_EXPR)
    {
      tree comp = build_new_op (info.loc, EQ_EXPR, flags, lhs, rhs,
				NULL_TREE, NULL_TREE, NULL, complain);
      comp = contextual_conv_bool (comp, complain);
      info.check (comp);
      if (defining)
	{
	  tree neg = build1 (TRUTH_NOT_EXPR, boolean_type_node, comp);
	  finish_return_stmt (neg);
	}
    }
  else
    {
      tree comp = build_new_op (info.loc, SPACESHIP_EXPR, flags, lhs, rhs,
				NULL_TREE, NULL_TREE, NULL, complain);
      tree comp2 = build_new_op (info.loc, code, flags, comp, integer_zero_node,
				 NULL_TREE, NULL_TREE, NULL, complain);
      info.check (comp2);
      if (defining)
	finish_return_stmt (comp2);
    }

 out:
  if (defining)
    finish_compound_stmt (compound_stmt);
  else
    --cp_unevaluated_operand;
}

/* True iff DECL is an implicitly-declared special member function with no real
   source location, so we can use its DECL_SOURCE_LOCATION to remember where we
   triggered its synthesis.  */

bool
decl_remember_implicit_trigger_p (tree decl)
{
  if (!DECL_ARTIFICIAL (decl))
    return false;
  special_function_kind sfk = special_function_p (decl);
  /* Inherited constructors have the location of their using-declaration, and
     operator== has the location of the corresponding operator<=>.  */
  return (sfk != sfk_inheriting_constructor
	  && sfk != sfk_comparison);
}

/* Synthesize FNDECL, a non-static member function.   */

void
synthesize_method (tree fndecl)
{
  bool need_body = true;
  tree stmt;
  location_t save_input_location = input_location;
  int error_count = errorcount;
  int warning_count = warningcount + werrorcount;
  special_function_kind sfk = special_function_p (fndecl);
  auto_diagnostic_group d;

  /* Reset the source location, we might have been previously
     deferred, and thus have saved where we were first needed.  */
  if (decl_remember_implicit_trigger_p (fndecl))
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

  bool push_to_top = maybe_push_to_top_level (fndecl);

  input_location = DECL_SOURCE_LOCATION (fndecl);

  start_preparsed_function (fndecl, NULL_TREE, SF_DEFAULT | SF_PRE_PARSED);
  stmt = begin_function_body ();

  if (DECL_ASSIGNMENT_OPERATOR_P (fndecl)
      && DECL_OVERLOADED_OPERATOR_IS (fndecl, NOP_EXPR))
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
  else if (sfk == sfk_comparison)
    {
      /* Pass tf_none so the function is just deleted if there's a problem.  */
      build_comparison_op (fndecl, true, tf_none);
      need_body = false;
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
  finish_function (/*inline_p=*/false);

  if (!DECL_DELETED_FN (fndecl))
    expand_or_defer_fn (fndecl);

  input_location = save_input_location;

  maybe_pop_from_top_level (push_to_top);

  pop_deferring_access_checks ();

  if (error_count != errorcount || warning_count != warningcount + werrorcount)
    if (DECL_ARTIFICIAL (fndecl))
      inform (input_location, "synthesized method %qD first required here",
	      fndecl);
}

/* Like synthesize_method, but don't actually synthesize defaulted comparison
   methods if their class is still incomplete.  Just deduce the return
   type in that case.  */

void
maybe_synthesize_method (tree fndecl)
{
  if (special_function_p (fndecl) == sfk_comparison)
    {
      tree lhs = DECL_ARGUMENTS (fndecl);
      if (is_this_parameter (lhs))
	lhs = cp_build_fold_indirect_ref (lhs);
      else
	lhs = convert_from_reference (lhs);
      tree ctype = TYPE_MAIN_VARIANT (TREE_TYPE (lhs));
      if (!COMPLETE_TYPE_P (ctype))
	{
	  push_deferring_access_checks (dk_no_deferred);
	  build_comparison_op (fndecl, false, tf_none);
	  pop_deferring_access_checks ();
	  return;
	}
    }
  return synthesize_method (fndecl);
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

tree
build_stub_object (tree reftype)
{
  if (!TYPE_REF_P (reftype))
    reftype = cp_build_reference_type (reftype, /*rval*/true);
  tree stub = build1 (CONVERT_EXPR, reftype, integer_one_node);
  return convert_from_reference (stub);
}

/* True iff EXPR is the result of build_stub_object.  */

bool
is_stub_object (tree expr)
{
  if (!REFERENCE_REF_P (expr))
    return false;
  expr = TREE_OPERAND (expr, 0);
  return (TREE_CODE (expr) == CONVERT_EXPR
	  && TREE_OPERAND (expr, 0) == integer_one_node);
}

/* Build a std::declval<TYPE>() expression and return it.  */

tree
build_trait_object (tree type)
{
  /* TYPE can't be a function with cv-/ref-qualifiers: std::declval is
     defined as

       template<class T>
       typename std::add_rvalue_reference<T>::type declval() noexcept;

     and std::add_rvalue_reference yields T when T is a function with
     cv- or ref-qualifiers, making the definition ill-formed.  */
  if (FUNC_OR_METHOD_TYPE_P (type)
      && (type_memfn_quals (type) != TYPE_UNQUALIFIED
	  || type_memfn_rqual (type) != REF_QUAL_NONE))
    return error_mark_node;

  return build_stub_object (type);
}

/* [func.require] Build an expression of INVOKE(FN_TYPE, ARG_TYPES...).  If the
   given is not invocable, returns error_mark_node.  */

tree
build_invoke (tree fn_type, const_tree arg_types, tsubst_flags_t complain)
{
  if (error_operand_p (fn_type) || error_operand_p (arg_types))
    return error_mark_node;

  gcc_assert (TYPE_P (fn_type));
  gcc_assert (TREE_CODE (arg_types) == TREE_VEC);

  /* Access check is required to determine if the given is invocable.  */
  deferring_access_check_sentinel acs (dk_no_deferred);

  /* INVOKE is an unevaluated context.  */
  cp_unevaluated cp_uneval_guard;

  bool is_ptrdatamem;
  bool is_ptrmemfunc;
  if (TREE_CODE (fn_type) == REFERENCE_TYPE)
    {
      tree non_ref_fn_type = TREE_TYPE (fn_type);
      is_ptrdatamem = TYPE_PTRDATAMEM_P (non_ref_fn_type);
      is_ptrmemfunc = TYPE_PTRMEMFUNC_P (non_ref_fn_type);

      /* Dereference fn_type if it is a pointer to member.  */
      if (is_ptrdatamem || is_ptrmemfunc)
	fn_type = non_ref_fn_type;
    }
  else
    {
      is_ptrdatamem = TYPE_PTRDATAMEM_P (fn_type);
      is_ptrmemfunc = TYPE_PTRMEMFUNC_P (fn_type);
    }

  if (is_ptrdatamem && TREE_VEC_LENGTH (arg_types) != 1)
    {
      if (complain & tf_error)
	error ("pointer to data member type %qT can only be invoked with "
	       "one argument", fn_type);
      return error_mark_node;
    }
  if (is_ptrmemfunc && TREE_VEC_LENGTH (arg_types) == 0)
    {
      if (complain & tf_error)
	error ("pointer to member function type %qT must be invoked with "
	       "at least one argument", fn_type);
      return error_mark_node;
    }

  /* Construct an expression of a pointer to member.  */
  tree ptrmem_expr;
  if (is_ptrdatamem || is_ptrmemfunc)
    {
      tree datum_type = TREE_VEC_ELT (arg_types, 0);
      tree non_ref_datum_type = datum_type;
      if (TYPE_REF_P (datum_type))
	non_ref_datum_type = TREE_TYPE (datum_type);

      /* datum must be a class type or a pointer to a class type.  */
      if (!CLASS_TYPE_P (non_ref_datum_type)
	  && !(POINTER_TYPE_P (non_ref_datum_type)
	       && CLASS_TYPE_P (TREE_TYPE (non_ref_datum_type))))
	{
	  if (complain & tf_error)
	    error ("first argument type %qT of a pointer to member must be a "
		   "class type or a pointer to a class type", datum_type);
	  return error_mark_node;
	}

      /* 1.1 & 1.4.  */
      tree ptrmem_class_type = TYPE_PTRMEM_CLASS_TYPE (fn_type);
      const bool ptrmem_is_same_or_base_of_datum =
	(same_type_ignoring_top_level_qualifiers_p (ptrmem_class_type,
						    non_ref_datum_type)
	 || (NON_UNION_CLASS_TYPE_P (ptrmem_class_type)
	     && NON_UNION_CLASS_TYPE_P (non_ref_datum_type)
	     && DERIVED_FROM_P (ptrmem_class_type, non_ref_datum_type)));

      bool datum_is_refwrap = false;
      if (!ptrmem_is_same_or_base_of_datum && CLASS_TYPE_P (non_ref_datum_type))
	{
	  tree datum_decl = TYPE_NAME (TYPE_MAIN_VARIANT (non_ref_datum_type));
	  if (decl_in_std_namespace_p (datum_decl))
	    {
	      const_tree name = DECL_NAME (datum_decl);
	      if (name && (id_equal (name, "reference_wrapper")))
		{
		  /* 1.2 & 1.5: Retrieve T from std::reference_wrapper<T>,
		     i.e., decltype(datum.get()).  */
		  datum_type =
		    TREE_VEC_ELT (TYPE_TI_ARGS (non_ref_datum_type), 0);
		  datum_is_refwrap = true;
		}
	    }
	}

      tree datum_expr = build_trait_object (datum_type);
      if (!ptrmem_is_same_or_base_of_datum && !datum_is_refwrap)
	/* 1.3 & 1.6: Try to dereference datum_expr.  */
	datum_expr = build_x_indirect_ref (UNKNOWN_LOCATION, datum_expr,
					   RO_UNARY_STAR, NULL_TREE, complain);

      tree fn_expr = build_trait_object (fn_type);
      ptrmem_expr = build_m_component_ref (datum_expr, fn_expr, complain);

      if (error_operand_p (ptrmem_expr))
	return error_mark_node;

      if (is_ptrdatamem)
	return ptrmem_expr;
    }

  /* Construct expressions for arguments to INVOKE.  For a pointer to member
     function, the first argument, which is the object, is not arguments to
     the function.  */
  releasing_vec args;
  for (int i = is_ptrmemfunc ? 1 : 0; i < TREE_VEC_LENGTH (arg_types); ++i)
    {
      tree arg_type = TREE_VEC_ELT (arg_types, i);
      tree arg = build_trait_object (arg_type);
      vec_safe_push (args, arg);
    }

  tree invoke_expr;
  if (is_ptrmemfunc)
    invoke_expr = build_offset_ref_call_from_tree (ptrmem_expr, &args,
						   complain);
  else  /* 1.7.  */
    invoke_expr = finish_call_expr (build_trait_object (fn_type), &args, false,
				    false, complain);
  return invoke_expr;
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

  if (TYPE_P (type))
    binfo = TYPE_BINFO (type);
  else
    {
      binfo = type;
      type = BINFO_TYPE (binfo);
    }

  ob = build_stub_object (cp_build_reference_type (type, false));
  releasing_vec args;
  if (argtype)
    {
      if (TREE_CODE (argtype) == TREE_LIST)
	{
	  for (tree elt = argtype; elt && elt != void_list_node;
	       elt = TREE_CHAIN (elt))
	    {
	      tree type = TREE_VALUE (elt);
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

  fns = lookup_fnfields (binfo, name, 0, complain);
  rval = build_new_method_call (ob, fns, &args, binfo, flags, &fn, complain);

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
  tree fn = locate_fn_flags (type, assign_op_identifier, argtype,
			     LOOKUP_NORMAL, tf_warning_or_error);
  if (fn == error_mark_node)
    return NULL_TREE;
  return fn;
}

/* walk_tree helper function for is_trivially_xible.  If *TP is a call,
   return it if it calls something other than a trivial special member
   function.  */

static tree
check_nontriv (tree *tp, int *, void *)
{
  tree fn = cp_get_callee (*tp);
  if (fn == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (fn) == ADDR_EXPR)
    fn = TREE_OPERAND (fn, 0);

  if (TREE_CODE (fn) != FUNCTION_DECL
      || !trivial_fn_p (fn))
    return fn;
  return NULL_TREE;
}

/* Return declval<T>() = declval<U>() treated as an unevaluated operand.  */

static tree
assignable_expr (tree to, tree from)
{
  cp_unevaluated cp_uneval_guard;
  to = build_trait_object (to);
  from = build_trait_object (from);
  tree r = cp_build_modify_expr (input_location, to, NOP_EXPR, from, tf_none);
  return r;
}

/* The predicate condition for a template specialization
   is_constructible<T, Args...> shall be satisfied if and only if the
   following variable definition would be well-formed for some invented
   variable t: T t(create<Args>()...);

   Return something equivalent in well-formedness and triviality.  */

static tree
constructible_expr (tree to, tree from)
{
  tree expr;
  cp_unevaluated cp_uneval_guard;
  const int len = TREE_VEC_LENGTH (from);
  if (CLASS_TYPE_P (to))
    {
      tree ctype = to;
      vec<tree, va_gc> *args = NULL;
      if (!TYPE_REF_P (to))
	to = cp_build_reference_type (to, /*rval*/false);
      tree ob = build_stub_object (to);
      if (len == 0)
	expr = build_value_init (ctype, tf_none);
      else
	{
	  vec_alloc (args, len);
	  for (tree arg : tree_vec_range (from))
	    args->quick_push (build_stub_object (arg));
	  expr = build_special_member_call (ob, complete_ctor_identifier, &args,
					    ctype, LOOKUP_NORMAL, tf_none);
	}
      if (expr == error_mark_node)
	return error_mark_node;
      /* The current state of the standard vis-a-vis LWG 2116 is that
	 is_*constructible involves destruction as well.  */
      if (type_build_dtor_call (ctype))
	{
	  tree dtor = build_special_member_call (ob, complete_dtor_identifier,
						 NULL, ctype, LOOKUP_NORMAL,
						 tf_none);
	  if (dtor == error_mark_node)
	    return error_mark_node;
	  if (!TYPE_HAS_TRIVIAL_DESTRUCTOR (ctype))
	    expr = build2 (COMPOUND_EXPR, void_type_node, expr, dtor);
	}
    }
  else
    {
      if (len == 0)
	return build_value_init (strip_array_types (to), tf_none);
      if (len > 1)
	{
	  if (cxx_dialect < cxx20)
	    /* Too many initializers.  */
	    return error_mark_node;

	  /* In C++20 this is well-formed:
	       using T = int[2];
	       T t(1, 2);
	     which means that std::is_constructible_v<int[2], int, int>
	     should be true.  */
	  vec<constructor_elt, va_gc> *v;
	  vec_alloc (v, len);
	  for (tree arg : tree_vec_range (from))
	    {
	      tree stub = build_stub_object (arg);
	      constructor_elt elt = { NULL_TREE, stub };
	      v->quick_push (elt);
	    }
	  from = build_constructor (init_list_type_node, v);
	  CONSTRUCTOR_IS_DIRECT_INIT (from) = true;
	  CONSTRUCTOR_IS_PAREN_INIT (from) = true;
	}
      else
	from = build_stub_object (TREE_VEC_ELT (from, 0));
      expr = perform_direct_initialization_if_possible (to, from,
							/*cast*/false,
							tf_none);
      /* If t(e) didn't work, maybe t{e} will.  */
      if (expr == NULL_TREE
	  && len == 1
	  && cxx_dialect >= cxx20)
	{
	  from = build_constructor_single (init_list_type_node, NULL_TREE,
					   from);
	  CONSTRUCTOR_IS_DIRECT_INIT (from) = true;
	  CONSTRUCTOR_IS_PAREN_INIT (from) = true;
	  expr = perform_direct_initialization_if_possible (to, from,
							    /*cast*/false,
							    tf_none);
	}
    }
  return expr;
}

/* Returns a tree iff TO is assignable (if CODE is MODIFY_EXPR) or
   constructible (otherwise) from FROM, which is a single type for
   assignment or a list of types for construction.  */

static tree
is_xible_helper (enum tree_code code, tree to, tree from, bool trivial)
{
  to = complete_type (to);
  deferring_access_check_sentinel acs (dk_no_deferred);
  if (VOID_TYPE_P (to) || ABSTRACT_CLASS_TYPE_P (to)
      || (from && FUNC_OR_METHOD_TYPE_P (from)
	  && (TYPE_READONLY (from) || FUNCTION_REF_QUALIFIED (from))))
    return error_mark_node;
  tree expr;
  if (code == MODIFY_EXPR)
    expr = assignable_expr (to, from);
  else if (trivial && TREE_VEC_LENGTH (from) > 1
	   && cxx_dialect < cxx20)
    return error_mark_node; // only 0- and 1-argument ctors can be trivial
			    // before C++20 aggregate paren init
  else if (TREE_CODE (to) == ARRAY_TYPE && !TYPE_DOMAIN (to))
    return error_mark_node; // can't construct an array of unknown bound
  else
    expr = constructible_expr (to, from);
  return expr;
}

/* Returns true iff TO is trivially assignable (if CODE is MODIFY_EXPR) or
   constructible (otherwise) from FROM, which is a single type for
   assignment or a list of types for construction.  */

bool
is_trivially_xible (enum tree_code code, tree to, tree from)
{
  tree expr = is_xible_helper (code, to, from, /*trivial*/true);
  if (expr == NULL_TREE || expr == error_mark_node)
    return false;
  tree nt = cp_walk_tree_without_duplicates (&expr, check_nontriv, NULL);
  return !nt;
}

/* Returns true iff TO is nothrow assignable (if CODE is MODIFY_EXPR) or
   constructible (otherwise) from FROM, which is a single type for
   assignment or a list of types for construction.  */

bool
is_nothrow_xible (enum tree_code code, tree to, tree from)
{
  ++cp_noexcept_operand;
  tree expr = is_xible_helper (code, to, from, /*trivial*/false);
  --cp_noexcept_operand;
  if (expr == NULL_TREE || expr == error_mark_node)
    return false;
  return expr_noexcept_p (expr, tf_none);
}

/* Returns true iff TO is assignable (if CODE is MODIFY_EXPR) or
   constructible (otherwise) from FROM, which is a single type for
   assignment or a list of types for construction.  */

bool
is_xible (enum tree_code code, tree to, tree from)
{
  tree expr = is_xible_helper (code, to, from, /*trivial*/false);
  if (expr == error_mark_node)
    return false;
  return !!expr;
}

/* Return true iff conjunction_v<is_reference<T>, is_constructible<T, U>> is
   true, and the initialization
     T t(VAL<U>); // DIRECT_INIT_P
   or
     T t = VAL<U>; // !DIRECT_INIT_P
   binds t to a temporary object whose lifetime is extended.
   VAL<T> is defined in [meta.unary.prop]:
   -- If T is a reference or function type, VAL<T> is an expression with the
   same type and value category as declval<T>().
   -- Otherwise, VAL<T> is a prvalue that initially has type T.   */

bool
ref_xes_from_temporary (tree to, tree from, bool direct_init_p)
{
  /* Check is_reference<T>.  */
  if (!TYPE_REF_P (to))
    return false;
  /* We don't check is_constructible<T, U>: if T isn't constructible
     from U, we won't be able to create a conversion.  */
  tree val = build_trait_object (from);
  if (val == error_mark_node)
    return false;
  if (!TYPE_REF_P (from) && TREE_CODE (from) != FUNCTION_TYPE)
    val = CLASS_TYPE_P (from) ? force_rvalue (val, tf_none) : rvalue (val);
  return ref_conv_binds_to_temporary (to, val, direct_init_p).is_true ();
}

/* Worker for is_{,nothrow_}convertible.  Attempt to perform an implicit
   conversion from FROM to TO and return the result.  */

static tree
is_convertible_helper (tree from, tree to)
{
  if (VOID_TYPE_P (from) && VOID_TYPE_P (to))
    return integer_one_node;
  cp_unevaluated u;
  tree expr = build_trait_object (from);
  /* std::is_{,nothrow_}convertible test whether the imaginary function
     definition

       To test() { return std::declval<From>(); }

     is well-formed.  A function can't return a function.  */
  if (FUNC_OR_METHOD_TYPE_P (to) || expr == error_mark_node)
    return error_mark_node;
  deferring_access_check_sentinel acs (dk_no_deferred);
  return perform_implicit_conversion (to, expr, tf_none);
}

/* Return true if FROM can be converted to TO using implicit conversions,
   or both FROM and TO are possibly cv-qualified void.  NB: This doesn't
   implement the "Access checks are performed as if from a context unrelated
   to either type" restriction.  */

bool
is_convertible (tree from, tree to)
{
  tree expr = is_convertible_helper (from, to);
  if (expr == error_mark_node)
    return false;
  return !!expr;
}

/* Like is_convertible, but the conversion is also noexcept.  */

bool
is_nothrow_convertible (tree from, tree to)
{
  tree expr = is_convertible_helper (from, to);
  if (expr == NULL_TREE || expr == error_mark_node)
    return false;
  return expr_noexcept_p (expr, tf_none);
}

/* Categorize various special_function_kinds.  */
#define SFK_CTOR_P(sfk) \
  ((sfk) >= sfk_constructor && (sfk) <= sfk_move_constructor)
#define SFK_DTOR_P(sfk) \
  ((sfk) == sfk_destructor || (sfk) == sfk_virtual_destructor)
#define SFK_ASSIGN_P(sfk) \
  ((sfk) == sfk_copy_assignment || (sfk) == sfk_move_assignment)
#define SFK_COPY_P(sfk) \
  ((sfk) == sfk_copy_constructor || (sfk) == sfk_copy_assignment)
#define SFK_MOVE_P(sfk) \
  ((sfk) == sfk_move_constructor || (sfk) == sfk_move_assignment)

/* Subroutine of synthesized_method_walk.  Update SPEC_P, TRIVIAL_P and
   DELETED_P or give an error message MSG with argument ARG.  */

static void
process_subob_fn (tree fn, special_function_kind sfk, tree *spec_p,
		  bool *trivial_p, bool *deleted_p, bool *constexpr_p,
		  bool diag, tree arg, bool dtor_from_ctor = false)
{
  if (!fn || fn == error_mark_node)
    {
      if (deleted_p)
	*deleted_p = true;
      return;
    }

  if (spec_p)
    {
      if (!maybe_instantiate_noexcept (fn))
	*spec_p = error_mark_node;
      else
	{
	  tree raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn));
	  *spec_p = merge_exception_specifiers (*spec_p, raises);
	}
    }

  if (!trivial_fn_p (fn) && !dtor_from_ctor)
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
	  inform (DECL_SOURCE_LOCATION (fn),
		  SFK_DTOR_P (sfk)
		  ? G_("defaulted destructor calls non-%<constexpr%> %qD")
		  : G_("defaulted constructor calls non-%<constexpr%> %qD"),
		  fn);
	  explain_invalid_constexpr_fn (fn);
	}
    }
}

/* Subroutine of synthesized_method_walk to allow recursion into anonymous
   aggregates.  If DTOR_FROM_CTOR is true, we're walking subobject destructors
   called from a synthesized constructor, in which case we don't consider
   the triviality of the subobject destructor.  */

static void
walk_field_subobs (tree fields, special_function_kind sfk, tree fnname,
		   int quals, tree *spec_p, bool *trivial_p,
		   bool *deleted_p, bool *constexpr_p,
		   bool diag, int flags, tsubst_flags_t complain,
		   bool dtor_from_ctor)
{
  if (!fields)
    return;

  tree ctx = DECL_CONTEXT (fields);

  /* CWG2084: A defaulted default ctor for a union with a DMI only initializes
     that member, so don't check other members.  */
  enum { unknown, no, yes }
  only_dmi_mem = (sfk == sfk_constructor && TREE_CODE (ctx) == UNION_TYPE
		  ? unknown : no);

 again:
  for (tree field = fields; field; field = DECL_CHAIN (field))
    {
      tree mem_type, argtype, rval;

      if (TREE_CODE (field) != FIELD_DECL
	  || DECL_ARTIFICIAL (field)
	  || DECL_UNNAMED_BIT_FIELD (field))
	continue;

      /* Variant members only affect deletedness.  In particular, they don't
	 affect the exception-specification of a user-provided destructor,
	 which we're figuring out via get_defaulted_eh_spec.  So if we aren't
	 asking if this is deleted, don't even look up the function; we don't
	 want an error about a deleted function we aren't actually calling.  */
      if (sfk == sfk_destructor && deleted_p == NULL
	  && TREE_CODE (ctx) == UNION_TYPE)
	break;

      if (only_dmi_mem != no)
	{
	  if (DECL_INITIAL (field))
	    only_dmi_mem = yes;
	  else
	    /* Don't check this until we know there's no DMI.  */
	    continue;
	}

      mem_type = strip_array_types (TREE_TYPE (field));
      if (SFK_ASSIGN_P (sfk))
	{
	  bool bad = true;
	  if (CP_TYPE_CONST_P (mem_type) && !CLASS_TYPE_P (mem_type))
	    {
	      if (diag)
		error ("non-static const member %q#D, cannot use default "
		       "assignment operator", field);
	    }
	  else if (TYPE_REF_P (mem_type))
	    {
	      if (diag)
		error ("non-static reference member %q#D, cannot use "
		       "default assignment operator", field);
	    }
	  else
	    bad = false;

	  if (bad && deleted_p)
	    *deleted_p = true;
	}
      else if (sfk == sfk_constructor || sfk == sfk_inheriting_constructor)
	{
	  bool bad;

	  if (DECL_INITIAL (field))
	    {
	      if (diag && DECL_INITIAL (field) == error_mark_node)
		inform (DECL_SOURCE_LOCATION (field),
			"initializer for %q#D is invalid", field);
	      if (trivial_p)
		*trivial_p = false;
	      /* Core 1351: If the field has an NSDMI that could throw, the
		 default constructor is noexcept(false).  */
	      if (spec_p)
		{
		  tree nsdmi = get_nsdmi (field, /*ctor*/false, complain);
		  if (nsdmi == error_mark_node)
		    *spec_p = error_mark_node;
		  else if (*spec_p != error_mark_node
			   && !expr_noexcept_p (nsdmi, tf_none))
		    *spec_p = noexcept_false_spec;
		}
	      /* Don't do the normal processing.  */
	      continue;
	    }

	  bad = false;
	  if (CP_TYPE_CONST_P (mem_type)
	      && default_init_uninitialized_part (mem_type))
	    {
	      if (diag)
		{
		  error ("uninitialized const member in %q#T",
			 current_class_type);
		  inform (DECL_SOURCE_LOCATION (field),
			  "%q#D should be initialized", field);
		}
	      bad = true;
	    }
	  else if (TYPE_REF_P (mem_type))
	    {
	      if (diag)
		{
		  error ("uninitialized reference member in %q#T",
			 current_class_type);
		  inform (DECL_SOURCE_LOCATION (field),
			  "%q#D should be initialized", field);
		}
	      bad = true;
	    }

	  if (bad && deleted_p)
	    *deleted_p = true;

	  /* Before C++20, for an implicitly-defined default constructor to
	     be constexpr, every member must have a user-provided default
	     constructor or an explicit initializer.  */
	  if (constexpr_p
	      && cxx_dialect < cxx20
	      && !CLASS_TYPE_P (mem_type)
	      && TREE_CODE (ctx) != UNION_TYPE)
	    {
	      *constexpr_p = false;
	      if (diag)
		inform (DECL_SOURCE_LOCATION (field),
			"defaulted default constructor does not "
			"initialize %q#D", field);
	    }
	}
      else if (sfk == sfk_copy_constructor)
	{
	  /* 12.8p11b5 */
	  if (TYPE_REF_P (mem_type)
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
	  walk_field_subobs (TYPE_FIELDS (mem_type), sfk, fnname, quals,
			     spec_p, trivial_p, deleted_p, constexpr_p,
			     diag, flags, complain, dtor_from_ctor);
	  continue;
	}

      if (SFK_COPY_P (sfk) || SFK_MOVE_P (sfk))
	{
	  int mem_quals = cp_type_quals (mem_type) | quals;
	  if (DECL_MUTABLE_P (field))
	    mem_quals &= ~TYPE_QUAL_CONST;
	  argtype = build_stub_type (mem_type, mem_quals, SFK_MOVE_P (sfk));
	}
      else
	argtype = NULL_TREE;

      rval = locate_fn_flags (mem_type, fnname, argtype, flags, complain);

      process_subob_fn (rval, sfk, spec_p, trivial_p, deleted_p,
			constexpr_p, diag, field, dtor_from_ctor);
    }

  /* We didn't find a DMI in this union, now check all the members.  */
  if (only_dmi_mem == unknown)
    {
      only_dmi_mem = no;
      goto again;
    }
}

/* Base walker helper for synthesized_method_walk.  Inspect a direct
   or virtual base.  BINFO is the parent type's binfo.  BASE_BINFO is
   the base binfo of interests.  All other parms are as for
   synthesized_method_walk, or its local vars.  */

static tree
synthesized_method_base_walk (tree binfo, tree base_binfo,
			      special_function_kind sfk, tree fnname, int quals,
			      tree *inheriting_ctor, tree inherited_parms,
			      int flags, bool diag,
			      tree *spec_p, bool *trivial_p,
			      bool *deleted_p, bool *constexpr_p)
{
  bool inherited_binfo = false;
  tree argtype = NULL_TREE;
  deferring_kind defer = dk_no_deferred;

  if (SFK_COPY_P (sfk) || SFK_MOVE_P (sfk))
    argtype = build_stub_type (BINFO_TYPE (base_binfo), quals, SFK_MOVE_P (sfk));
  else if (inheriting_ctor
	   && (inherited_binfo
	       = binfo_inherited_from (binfo, base_binfo, *inheriting_ctor)))
    {
      argtype = inherited_parms;
      /* Don't check access on the inherited constructor.  */
      if (flag_new_inheriting_ctors)
	defer = dk_deferred;
    }
  else if (cxx_dialect >= cxx14 && sfk == sfk_virtual_destructor
	   && BINFO_VIRTUAL_P (base_binfo)
	   && ABSTRACT_CLASS_TYPE_P (BINFO_TYPE (binfo)))
    /* Don't check access when looking at vbases of abstract class's
       virtual destructor.  */
    defer = dk_no_check;

  if (defer != dk_no_deferred)
    push_deferring_access_checks (defer);
  tree rval = locate_fn_flags (base_binfo, fnname, argtype, flags,
			       diag ? tf_warning_or_error : tf_none);
  if (defer != dk_no_deferred)
    pop_deferring_access_checks ();

  /* Replace an inherited template with the appropriate specialization.  */
  if (inherited_binfo && rval
      && DECL_P (*inheriting_ctor) && DECL_P (rval)
      && DECL_CONTEXT (*inheriting_ctor) == DECL_CONTEXT (rval))
    *inheriting_ctor = DECL_CLONED_FUNCTION (rval);

  process_subob_fn (rval, sfk, spec_p, trivial_p, deleted_p,
		    constexpr_p, diag, BINFO_TYPE (base_binfo));
  if (SFK_CTOR_P (sfk)
      && (!BINFO_VIRTUAL_P (base_binfo)
	  || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (BINFO_TYPE (base_binfo))))
    {
      /* In a constructor we also need to check the subobject
	 destructors for cleanup of partially constructed objects.  */
      tree dtor = locate_fn_flags (base_binfo, complete_dtor_identifier,
				   NULL_TREE, flags,
				   diag ? tf_warning_or_error : tf_none);
      /* Note that we don't pass down trivial_p; the subobject
	 destructors don't affect triviality of the constructor.  Nor
	 do they affect constexpr-ness (a constant expression doesn't
	 throw) or exception-specification (a throw from one of the
	 dtors would be a double-fault).  */
      process_subob_fn (dtor, sfk, NULL, NULL, deleted_p, NULL, false,
			BINFO_TYPE (base_binfo), /*dtor_from_ctor*/true);
    }

  return rval;
}

/* The caller wants to generate an implicit declaration of SFK for
   CTYPE which is const if relevant and CONST_P is set.  If SPEC_P,
   TRIVIAL_P, DELETED_P or CONSTEXPR_P are non-null, set their
   referent appropriately.  If DIAG is true, we're either being called
   from maybe_explain_implicit_delete to give errors, or if
   CONSTEXPR_P is non-null, from explain_invalid_constexpr_fn.  */

static void
synthesized_method_walk (tree ctype, special_function_kind sfk, bool const_p,
			 tree *spec_p, bool *trivial_p, bool *deleted_p,
			 bool *constexpr_p, bool diag,
			 tree *inheriting_ctor, tree inherited_parms)
{
  tree binfo, base_binfo;
  int i;

  /* SFK must be exactly one category.  */
  gcc_checking_assert (SFK_DTOR_P(sfk) + SFK_CTOR_P(sfk)
		       + SFK_ASSIGN_P(sfk) == 1);

  if (spec_p)
    *spec_p = (cxx_dialect >= cxx11 ? noexcept_true_spec : empty_except_spec);

  if (deleted_p)
    {
      /* "The closure type associated with a lambda-expression has a deleted
	 default constructor and a deleted copy assignment operator."
	 This is diagnosed in maybe_explain_implicit_delete.
	 In C++20, only lambda-expressions with lambda-captures have those
	 deleted.  */
      if (LAMBDA_TYPE_P (ctype)
	  && (sfk == sfk_constructor || sfk == sfk_copy_assignment)
	  && (cxx_dialect < cxx20
	      || LAMBDA_EXPR_CAPTURE_LIST (CLASSTYPE_LAMBDA_EXPR (ctype))
	      || LAMBDA_EXPR_DEFAULT_CAPTURE_MODE
				(CLASSTYPE_LAMBDA_EXPR (ctype)) != CPLD_NONE))
	{
	  *deleted_p = true;
	  return;
	}

      *deleted_p = false;
    }

  bool check_vdtor = false;
  tree fnname;

  if (SFK_DTOR_P (sfk))
    {
      check_vdtor = true;
      /* The synthesized method will call base dtors, but check complete
	 here to avoid having to deal with VTT.  */
      fnname = complete_dtor_identifier;
    }
  else if (SFK_ASSIGN_P (sfk))
    fnname = assign_op_identifier;
  else
    fnname = complete_ctor_identifier;

  gcc_assert ((sfk == sfk_inheriting_constructor)
	      == (inheriting_ctor && *inheriting_ctor != NULL_TREE));

  /* If that user-written default constructor would satisfy the
     requirements of a constexpr constructor (7.1.5), the
     implicitly-defined default constructor is constexpr.

     C++20:
     The implicitly-defined copy/move assignment operator is constexpr if
      - X is a literal type, and
      - the assignment operator selected to copy/move each direct base class
	subobject is a constexpr function, and
      - for each non-static data member of X that is of class type (or array
	thereof), the assignment operator selected to copy/move that
	member is a constexpr function.

      C++23:
      The implicitly-defined copy/move assignment operator is constexpr.  */
  if (constexpr_p)
    *constexpr_p = (SFK_CTOR_P (sfk)
		    || (SFK_ASSIGN_P (sfk) && cxx_dialect >= cxx14)
		    || (SFK_DTOR_P (sfk) && cxx_dialect >= cxx20));

  bool expected_trivial = type_has_trivial_fn (ctype, sfk);
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
      && (!(SFK_COPY_P (sfk) || SFK_MOVE_P (sfk)) || cxx_dialect < cxx11))
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

  bool push_to_top = maybe_push_to_top_level (TYPE_NAME (ctype));
  ++cp_unevaluated_operand;
  ++c_inhibit_evaluation_warnings;
  push_deferring_access_checks (dk_no_deferred);

  tree scope = push_scope (ctype);

  int flags = LOOKUP_NORMAL | LOOKUP_SPECULATIVE;
  if (sfk != sfk_inheriting_constructor)
    flags |= LOOKUP_DEFAULTED;

  tsubst_flags_t complain = diag ? tf_warning_or_error : tf_none;
  if (diag && spec_p)
    /* We're in get_defaulted_eh_spec; we don't actually want any walking
       diagnostics, we just want complain set.  */
    diag = false;
  int quals = const_p ? TYPE_QUAL_CONST : TYPE_UNQUALIFIED;

  for (binfo = TYPE_BINFO (ctype), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    {
      if (!SFK_ASSIGN_P (sfk) && BINFO_VIRTUAL_P (base_binfo))
	/* We'll handle virtual bases below.  */
	continue;

      tree fn = synthesized_method_base_walk (binfo, base_binfo,
					      sfk, fnname, quals,
					      inheriting_ctor, inherited_parms,
					      flags, diag, spec_p, trivial_p,
					      deleted_p, constexpr_p);

      if (diag && SFK_ASSIGN_P (sfk) && SFK_MOVE_P (sfk)
	  && BINFO_VIRTUAL_P (base_binfo)
	  && fn && TREE_CODE (fn) == FUNCTION_DECL
	  && move_fn_p (fn) && !trivial_fn_p (fn)
	  && vbase_has_user_provided_move_assign (BINFO_TYPE (base_binfo)))
	warning (OPT_Wvirtual_move_assign,
		 "defaulted move assignment for %qT calls a non-trivial "
		 "move assignment operator for virtual base %qT",
		 ctype, BINFO_TYPE (base_binfo));

      if (check_vdtor && type_has_virtual_destructor (BINFO_TYPE (base_binfo)))
	{
	  /* Unlike for base ctor/op=/dtor, for operator delete it's fine
	     to have a null fn (no class-specific op delete).  */
	  fn = locate_fn_flags (ctype, ovl_op_identifier (false, DELETE_EXPR),
				ptr_type_node, flags, tf_none);
	  if (fn && fn == error_mark_node)
	    {
	      if (complain & tf_error)
		locate_fn_flags (ctype, ovl_op_identifier (false, DELETE_EXPR),
				 ptr_type_node, flags, complain);
	      if (deleted_p)
		*deleted_p = true;
	    }
	  check_vdtor = false;
	}
    }

  vec<tree, va_gc> *vbases = CLASSTYPE_VBASECLASSES (ctype);
  if (SFK_ASSIGN_P (sfk))
    /* Already examined vbases above.  */;
  else if (vec_safe_is_empty (vbases))
    /* No virtual bases to worry about.  */;
  else if (ABSTRACT_CLASS_TYPE_P (ctype) && cxx_dialect >= cxx14
	   /* DR 1658 specifies that vbases of abstract classes are
	      ignored for both ctors and dtors.  Except DR 2336
	      overrides that skipping when determing the eh-spec of a
	      virtual destructor.  */
	   && sfk != sfk_virtual_destructor)
    /* Vbase cdtors are not relevant.  */;
  else
    {
      if (constexpr_p)
	*constexpr_p = false;

      FOR_EACH_VEC_ELT (*vbases, i, base_binfo)
	synthesized_method_base_walk (binfo, base_binfo, sfk, fnname, quals,
				      inheriting_ctor, inherited_parms,
				      flags, diag,
				      spec_p, trivial_p, deleted_p, constexpr_p);
    }

  /* Now handle the non-static data members.  */
  walk_field_subobs (TYPE_FIELDS (ctype), sfk, fnname, quals,
		     spec_p, trivial_p, deleted_p, constexpr_p,
		     diag, flags, complain, /*dtor_from_ctor*/false);
  if (SFK_CTOR_P (sfk))
    walk_field_subobs (TYPE_FIELDS (ctype), sfk_destructor,
		       complete_dtor_identifier, TYPE_UNQUALIFIED,
		       NULL, NULL, deleted_p, NULL,
		       false, flags, complain, /*dtor_from_ctor*/true);

  pop_scope (scope);

  pop_deferring_access_checks ();
  --cp_unevaluated_operand;
  --c_inhibit_evaluation_warnings;
  maybe_pop_from_top_level (push_to_top);
}

/* DECL is a defaulted function whose exception specification is now
   needed.  Return what it should be.  */

tree
get_defaulted_eh_spec (tree decl, tsubst_flags_t complain)
{
  /* For DECL_MAYBE_DELETED this should already have been handled by
     synthesize_method.  */
  gcc_assert (!DECL_MAYBE_DELETED (decl));

  if (DECL_CLONED_FUNCTION_P (decl))
    decl = DECL_CLONED_FUNCTION (decl);
  special_function_kind sfk = special_function_p (decl);
  tree ctype = DECL_CONTEXT (decl);
  tree parms = FUNCTION_FIRST_USER_PARMTYPE (decl);
  tree parm_type = TREE_VALUE (parms);
  bool const_p = CP_TYPE_CONST_P (non_reference (parm_type));
  tree spec = empty_except_spec;
  bool diag = !DECL_DELETED_FN (decl) && (complain & tf_error);
  tree inh = DECL_INHERITED_CTOR (decl);
  if (SFK_DTOR_P (sfk) && DECL_VIRTUAL_P (decl))
    /* We have to examine virtual bases even if abstract.  */
    sfk = sfk_virtual_destructor;
  bool pushed = false;
  if (CLASSTYPE_TEMPLATE_INSTANTIATION (ctype))
    pushed = push_tinst_level (decl);
  synthesized_method_walk (ctype, sfk, const_p, &spec, NULL, NULL,
			   NULL, diag, &inh, parms);
  if (pushed)
    pop_tinst_level ();
  return spec;
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
      static hash_set<tree> *explained;

      special_function_kind sfk;
      location_t loc;
      bool informed;
      tree ctype;

      if (!explained)
	explained = new hash_set<tree>;
      if (explained->add (decl))
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
	       && (sfk == sfk_copy_assignment || sfk == sfk_copy_constructor)
	       && classtype_has_move_assign_or_move_ctor_p (ctype, true))
	{
	  inform (DECL_SOURCE_LOCATION (decl),
		  "%q#D is implicitly declared as deleted because %qT "
		  "declares a move constructor or move assignment operator",
		  decl, ctype);
	  informed = true;
	}
      else if (sfk == sfk_inheriting_constructor)
	{
	  tree binfo = inherited_ctor_binfo (decl);
	  if (TREE_CODE (binfo) != TREE_BINFO)
	    {
	      inform (DECL_SOURCE_LOCATION (decl),
		      "%q#D inherits from multiple base subobjects",
		      decl);
	      informed = true;
	    }
	}
      if (!informed && sfk == sfk_comparison)
	{
	  inform (DECL_SOURCE_LOCATION (decl),
		  "%q#D is implicitly deleted because the default "
		  "definition would be ill-formed:", decl);
	  build_comparison_op (decl, false, tf_warning_or_error);
	}
      else if (!informed)
	{
	  tree parms = FUNCTION_FIRST_USER_PARMTYPE (decl);
	  bool const_p = false;
	  if (parms)
	    {
	      tree parm_type = TREE_VALUE (parms);
	      const_p = CP_TYPE_CONST_P (non_reference (parm_type));
	    }
	  tree raises = NULL_TREE;
	  bool deleted_p = false;
	  tree scope = push_scope (ctype);
	  tree inh = DECL_INHERITED_CTOR (decl);

	  synthesized_method_walk (ctype, sfk, const_p,
				   &raises, NULL, &deleted_p, NULL, false,
				   &inh, parms);
	  if (deleted_p)
	    {
	      inform (DECL_SOURCE_LOCATION (decl),
		      "%q#D is implicitly deleted because the default "
		      "definition would be ill-formed:", decl);
	      synthesized_method_walk (ctype, sfk, const_p,
				       NULL, NULL, &deleted_p, NULL, true,
				       &inh, parms);
	    }
	  else if (!comp_except_specs
		   (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (decl)),
		    raises, ce_normal))
	    inform (DECL_SOURCE_LOCATION (decl), "%q#F is implicitly "
		    "deleted because its exception-specification does not "
		    "match the implicit exception-specification %qX",
		    decl, raises);
	  else if (flag_checking)
	    gcc_unreachable ();

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
  tree parms = FUNCTION_FIRST_USER_PARMTYPE (decl);
  bool const_p = CP_TYPE_CONST_P (non_reference (TREE_VALUE (parms)));
  tree inh = DECL_INHERITED_CTOR (decl);
  bool dummy;
  special_function_kind sfk = special_function_p (decl);
  if (sfk == sfk_comparison)
    {
      DECL_DECLARED_CONSTEXPR_P (decl) = true;
      build_comparison_op (decl, false, tf_warning_or_error);
      DECL_DECLARED_CONSTEXPR_P (decl) = false;
    }
  else
    synthesized_method_walk (DECL_CLASS_CONTEXT (decl),
			     sfk, const_p,
			     NULL, NULL, NULL, &dummy, true,
			     &inh, parms);
}

/* DECL is an instantiation of an inheriting constructor template.  Deduce
   the correct exception-specification and deletedness for this particular
   specialization.  Return true if the deduction succeeds; false otherwise.  */

bool
deduce_inheriting_ctor (tree decl)
{
  decl = DECL_ORIGIN (decl);
  gcc_assert (DECL_INHERITED_CTOR (decl));
  tree spec;
  bool trivial, constexpr_, deleted;
  tree inh = DECL_INHERITED_CTOR (decl);
  synthesized_method_walk (DECL_CONTEXT (decl), sfk_inheriting_constructor,
			   false, &spec, &trivial, &deleted, &constexpr_,
			   /*diag*/false,
			   &inh,
			   FUNCTION_FIRST_USER_PARMTYPE (decl));
  if (spec == error_mark_node)
    return false;
  if (TREE_CODE (inherited_ctor_binfo (decl)) != TREE_BINFO)
    /* Inherited the same constructor from different base subobjects.  */
    deleted = true;
  DECL_DELETED_FN (decl) = deleted;
  TREE_TYPE (decl) = build_exception_variant (TREE_TYPE (decl), spec);
  SET_DECL_INHERITED_CTOR (decl, inh);

  tree clone;
  FOR_EACH_CLONE (clone, decl)
    {
      DECL_DELETED_FN (clone) = deleted;
      TREE_TYPE (clone) = build_exception_variant (TREE_TYPE (clone), spec);
      SET_DECL_INHERITED_CTOR (clone, inh);
    }

  return true;
}

/* Implicitly declare the special function indicated by KIND, as a
   member of TYPE.  For copy constructors and assignment operators,
   CONST_P indicates whether these functions should take a const
   reference argument or a non-const reference.
   Returns the FUNCTION_DECL for the implicitly declared function.  */

tree
implicitly_declare_fn (special_function_kind kind, tree type,
		       bool const_p, tree pattern_fn,
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
  bool deleted_p = false;
  bool constexpr_p = false;
  tree inherited_ctor = (kind == sfk_inheriting_constructor
			 ? pattern_fn : NULL_TREE);

  /* Because we create declarations for implicitly declared functions
     lazily, we may be creating the declaration for a member of TYPE
     while in some completely different context.  However, TYPE will
     never be a dependent class (because we never want to do lookups
     for implicitly defined functions in a dependent class).  */
  gcc_assert (!dependent_type_p (type));

  /* If the member-specification does not explicitly declare any member or
     friend named operator==, an == operator function is declared
     implicitly for each three-way comparison operator function defined as
     defaulted in the member-specification, with the same access and
     function-definition and in the same class scope as the respective
     three-way comparison operator function, except that the return type is
     replaced with bool and the declarator-id is replaced with
     operator==.

     [Note: Such an implicitly-declared == operator for a class X is
     defined as defaulted in the definition of X and has the same
     parameter-declaration-clause and trailing requires-clause as the
     respective three-way comparison operator. It is declared with friend,
     virtual, constexpr, or consteval if the three-way comparison operator
     function is so declared. If the three-way comparison operator function
     has no noexcept-specifier, the implicitly-declared == operator
     function has an implicit exception specification (14.5) that may
     differ from the implicit exception specification of the three-way
     comparison operator function. --end note]  */
  if (kind == sfk_comparison)
    {
      fn = copy_operator_fn (pattern_fn, EQ_EXPR);
      DECL_ARTIFICIAL (fn) = 1;
      apply_deduced_return_type (fn, boolean_type_node);
      return fn;
    }

  /* Furthermore, we must set PROCESSING_TEMPLATE_DECL to zero here
     because we only create clones for constructors and destructors
     when not in a template.  */
  saved_processing_template_decl = processing_template_decl;
  processing_template_decl = 0;

  type = TYPE_MAIN_VARIANT (type);

  if (targetm.cxx.cdtor_returns_this ())
    {
      if (kind == sfk_destructor)
	/* See comment in check_special_function_return_type.  */
	return_type = build_pointer_type (void_type_node);
      else
	return_type = build_pointer_type (type);
    }
  else
    return_type = void_type_node;

  int this_quals = TYPE_UNQUALIFIED;
  switch (kind)
    {
    case sfk_destructor:
      /* Destructor.  */
      name = dtor_identifier;
      break;

    case sfk_constructor:
      /* Default constructor.  */
      name = ctor_identifier;
      break;

    case sfk_copy_constructor:
    case sfk_copy_assignment:
    case sfk_move_constructor:
    case sfk_move_assignment:
    case sfk_inheriting_constructor:
    {
      if (kind == sfk_copy_assignment
	  || kind == sfk_move_assignment)
	{
	  return_type = build_reference_type (type);
	  name = assign_op_identifier;
	}
      else
	name = ctor_identifier;

      if (kind == sfk_inheriting_constructor)
	parameter_types = inherited_parms;
      else
	{
	  if (const_p)
	    rhs_parm_type = cp_build_qualified_type (type, TYPE_QUAL_CONST);
	  else
	    rhs_parm_type = type;
	  bool move_p = (kind == sfk_move_assignment
			 || kind == sfk_move_constructor);
	  rhs_parm_type = cp_build_reference_type (rhs_parm_type, move_p);

	  parameter_types = tree_cons (NULL_TREE, rhs_parm_type, parameter_types);
	}
      break;
    }

    default:
      gcc_unreachable ();
    }

  bool trivial_p = false;

  if (inherited_ctor)
    {
      /* For an inheriting constructor, just copy these flags from the
	 inherited constructor until deduce_inheriting_ctor.  */
      raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (inherited_ctor));
      deleted_p = DECL_DELETED_FN (inherited_ctor);
      constexpr_p = DECL_DECLARED_CONSTEXPR_P (inherited_ctor);
    }
  else if (cxx_dialect >= cxx11)
    {
      raises = noexcept_deferred_spec;
      synthesized_method_walk (type, kind, const_p, NULL, &trivial_p,
			       &deleted_p, &constexpr_p, false,
			       &inherited_ctor, inherited_parms);
    }
  else
    synthesized_method_walk (type, kind, const_p, &raises, &trivial_p,
			     &deleted_p, &constexpr_p, false,
			     &inherited_ctor, inherited_parms);
  /* Don't bother marking a deleted constructor as constexpr.  */
  if (deleted_p)
    constexpr_p = false;
  /* A trivial copy/move constructor is also a constexpr constructor,
     unless the class has virtual bases (7.1.5p4).  */
  else if (trivial_p
	   && cxx_dialect >= cxx11
	   && (kind == sfk_copy_constructor
	       || kind == sfk_move_constructor)
	   && !CLASSTYPE_VBASECLASSES (type))
    gcc_assert (constexpr_p);

  if (!trivial_p && type_has_trivial_fn (type, kind))
    type_set_nontrivial_flag (type, kind);

  /* Create the function.  */
  tree this_type = cp_build_qualified_type (type, this_quals);
  fn_type = build_method_type_directly (this_type, return_type,
					parameter_types);

  if (raises)
    {
      if (raises != error_mark_node)
	fn_type = build_exception_variant (fn_type, raises);
      else
	{
	  /* Can happen, e.g., in C++98 mode for an ill-formed non-static data
	     member initializer (c++/89914).  Also, in C++98, we might have
	     failed to deduce RAISES, so try again but complain this time.  */
	  if (cxx_dialect < cxx11)
	    synthesized_method_walk (type, kind, const_p, &raises, nullptr,
				     nullptr, nullptr, /*diag=*/true,
				     &inherited_ctor, inherited_parms);
	  /* We should have seen an error at this point.  */
	  gcc_assert (seen_error ());
	}
    }
  fn = build_lang_decl (FUNCTION_DECL, name, fn_type);
  if (kind != sfk_inheriting_constructor)
    DECL_SOURCE_LOCATION (fn) = DECL_SOURCE_LOCATION (TYPE_NAME (type));

  if (IDENTIFIER_OVL_OP_P (name))
    {
      const ovl_op_info_t *op = IDENTIFIER_OVL_OP_INFO (name);
      DECL_OVERLOADED_OPERATOR_CODE_RAW (fn) = op->ovl_op_code;
    }
  else if (IDENTIFIER_CTOR_P (name))
    DECL_CXX_CONSTRUCTOR_P (fn) = true;
  else if (IDENTIFIER_DTOR_P (name))
    DECL_CXX_DESTRUCTOR_P (fn) = true;
  else
    gcc_unreachable ();

  SET_DECL_ALIGN (fn, MINIMUM_METHOD_BOUNDARY);

  /* Create the explicit arguments.  */
  if (rhs_parm_type)
    {
      /* Note that this parameter is *not* marked DECL_ARTIFICIAL; we
	 want its type to be included in the mangled function
	 name.  */
      tree decl = cp_build_parm_decl (fn, NULL_TREE, rhs_parm_type);
      TREE_READONLY (decl) = 1;
      retrofit_lang_decl (decl);
      DECL_PARM_INDEX (decl) = DECL_PARM_LEVEL (decl) = 1;
      DECL_ARGUMENTS (fn) = decl;
    }
  else if (kind == sfk_inheriting_constructor)
    {
      tree *p = &DECL_ARGUMENTS (fn);
      int index = 1;
      for (tree parm = inherited_parms; parm && parm != void_list_node;
	   parm = TREE_CHAIN (parm))
	{
	  *p = cp_build_parm_decl (fn, NULL_TREE, TREE_VALUE (parm));
	  retrofit_lang_decl (*p);
	  DECL_PARM_LEVEL (*p) = 1;
	  DECL_PARM_INDEX (*p) = index++;
	  p = &DECL_CHAIN (*p);
	}
      SET_DECL_INHERITED_CTOR (fn, inherited_ctor);
      DECL_NONCONVERTING_P (fn) = DECL_NONCONVERTING_P (inherited_ctor);
      /* A constructor so declared has the same access as the corresponding
	 constructor in X.  */
      TREE_PRIVATE (fn) = TREE_PRIVATE (inherited_ctor);
      TREE_PROTECTED (fn) = TREE_PROTECTED (inherited_ctor);
      /* Copy constexpr from the inherited constructor even if the
	 inheriting constructor doesn't satisfy the requirements.  */
      constexpr_p = DECL_DECLARED_CONSTEXPR_P (inherited_ctor);
      tree inherited_ctor_fn = STRIP_TEMPLATE (inherited_ctor);
      /* Also copy any attributes.  */
      DECL_ATTRIBUTES (fn) = clone_attrs (DECL_ATTRIBUTES (inherited_ctor_fn));
      DECL_DISREGARD_INLINE_LIMITS (fn)
	= DECL_DISREGARD_INLINE_LIMITS (inherited_ctor_fn);
    }

  /* Add the "this" parameter.  */
  this_parm = build_this_parm (fn, fn_type, this_quals);
  DECL_CHAIN (this_parm) = DECL_ARGUMENTS (fn);
  DECL_ARGUMENTS (fn) = this_parm;

  grokclassfn (type, fn, kind == sfk_destructor ? DTOR_FLAG : NO_SPECIAL);

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
  set_linkage_according_to_type (type, fn);
  if (TREE_PUBLIC (fn))
    DECL_COMDAT (fn) = 1;
  rest_of_decl_compilation (fn, namespace_bindings_p (), at_eof);
  gcc_assert (!TREE_USED (fn));

  /* Propagate constraints from the inherited constructor. */
  if (flag_concepts && inherited_ctor)
    if (tree orig_ci = get_constraints (inherited_ctor))
      {
        tree new_ci = copy_node (orig_ci);
        set_constraints (fn, new_ci);
      }

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
			       NULL, NULL_TREE);
      input_location = loc;
    }

  return fn;
}

/* Mark an explicitly defaulted function FN as =deleted and warn.
   IMPLICIT_FN is the corresponding special member function that
   would have been implicitly declared.  */

void
maybe_delete_defaulted_fn (tree fn, tree implicit_fn)
{
  if (DECL_ARTIFICIAL (fn) || !DECL_DEFAULTED_IN_CLASS_P (fn))
    return;

  DECL_DELETED_FN (fn) = true;

  auto_diagnostic_group d;
  const special_function_kind kind = special_function_p (fn);
  tree parmtype
    = TREE_VALUE (DECL_XOBJ_MEMBER_FUNCTION_P (fn)
		  ? TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (fn)))
		  : FUNCTION_FIRST_USER_PARMTYPE (fn));
  const bool illformed_p
    /* [dcl.fct.def.default] "if F1 is an assignment operator"...  */
    = (SFK_ASSIGN_P (kind)
       /* "and the return type of F1 differs from the return type of F2"  */
       && (!same_type_p (TREE_TYPE (TREE_TYPE (fn)),
			 TREE_TYPE (TREE_TYPE (implicit_fn)))
	   /* "or F1's non-object parameter type is not a reference,
	      the program is ill-formed"  */
	   || !TYPE_REF_P (parmtype)));
  /* Decide if we want to emit a pedwarn, error, or a warning.  */
  diagnostic_t diag_kind;
  int opt;
  if (illformed_p)
    {
      diag_kind = DK_ERROR;
      opt = 0;
    }
  else
    {
      diag_kind = cxx_dialect >= cxx20 ? DK_WARNING : DK_PEDWARN;
      opt = OPT_Wdefaulted_function_deleted;
    }

  /* Don't warn for template instantiations.  */
  if (DECL_TEMPLATE_INSTANTIATION (fn) && diag_kind == DK_WARNING)
    return;

  const char *wmsg;
  switch (kind)
    {
    case sfk_copy_constructor:
      wmsg = G_("explicitly defaulted copy constructor is implicitly deleted "
		"because its declared type does not match the type of an "
		"implicit copy constructor");
      break;
    case sfk_move_constructor:
      wmsg = G_("explicitly defaulted move constructor is implicitly deleted "
		"because its declared type does not match the type of an "
		"implicit move constructor");
      break;
    case sfk_copy_assignment:
      wmsg = G_("explicitly defaulted copy assignment operator is implicitly "
		"deleted because its declared type does not match the type "
		"of an implicit copy assignment operator");
      break;
    case sfk_move_assignment:
      wmsg = G_("explicitly defaulted move assignment operator is implicitly "
		"deleted because its declared type does not match the type "
		"of an implicit move assignment operator");
      break;
    default:
      gcc_unreachable ();
    }
  if (emit_diagnostic (diag_kind, DECL_SOURCE_LOCATION (fn), opt, wmsg))
    inform (DECL_SOURCE_LOCATION (fn),
	    "expected signature: %qD", implicit_fn);
}

/* Gives any errors about defaulted functions which need to be deferred
   until the containing class is complete.  IMP_CONST is false or true
   if we are called from check_bases_and_members and signals whether
   the implicit function has a non-object parameter of type const C&.  */

void
defaulted_late_check (tree fn, tristate imp_const/*=tristate::unknown()*/)
{
  /* Complain about invalid signature for defaulted fn.  */
  tree ctx = DECL_CONTEXT (fn);
  special_function_kind kind = special_function_p (fn);

  if (kind == sfk_comparison)
    {
      /* If the function was declared constexpr, check that the definition
	 qualifies.  Otherwise we can define the function lazily.  */
      if (DECL_DECLARED_CONSTEXPR_P (fn) && !DECL_INITIAL (fn))
	{
	  /* Prevent GC.  */
	  function_depth++;
	  synthesize_method (fn);
	  function_depth--;
	}
      return;
    }

  bool fn_const_p = (copy_fn_p (fn) == 2);
  /* "if F2 has a non-object parameter of type const C&, the corresponding
     non-object parameter of F1 may be of type C&."  But not the other way
     around.  */
  if (fn_const_p && imp_const.is_false ())
    fn_const_p = false;
  tree implicit_fn = implicitly_declare_fn (kind, ctx, fn_const_p,
					    /*pattern_fn=*/NULL_TREE,
					    /*inherited_parms=*/NULL_TREE);
  tree eh_spec = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (implicit_fn));

  /* Includes special handling for a default xobj operator.  */
  auto compare_fn_params = [](tree fn, tree implicit_fn){
    tree fn_parms = TYPE_ARG_TYPES (TREE_TYPE (fn));
    tree implicit_fn_parms = TYPE_ARG_TYPES (TREE_TYPE (implicit_fn));

    if (DECL_XOBJ_MEMBER_FUNCTION_P (fn))
      {
	tree fn_obj_ref_type = TREE_VALUE (fn_parms);
	/* We can't default xobj operators with an xobj parameter that is not
	   an lvalue reference, even if it would correspond.  */
	if (!TYPE_REF_P (fn_obj_ref_type)
	    || TYPE_REF_IS_RVALUE (fn_obj_ref_type)
	    || !object_parms_correspond (fn, implicit_fn,
					 DECL_CONTEXT (implicit_fn)))
	  return false;
	/* We just compared the object parameters, skip over them before
	   passing to compparms.  */
	fn_parms = TREE_CHAIN (fn_parms);
	implicit_fn_parms = TREE_CHAIN (implicit_fn_parms);
      }
    return compparms (fn_parms, implicit_fn_parms);
  };

  if (!same_type_p (TREE_TYPE (TREE_TYPE (fn)),
		    TREE_TYPE (TREE_TYPE (implicit_fn)))
      || !compare_fn_params (fn, implicit_fn))
    maybe_delete_defaulted_fn (fn, implicit_fn);

  if (DECL_DELETED_FN (implicit_fn))
    {
      DECL_DELETED_FN (fn) = 1;
      return;
    }

  /* If a function is explicitly defaulted on its first declaration without an
     exception-specification, it is implicitly considered to have the same
     exception-specification as if it had been implicitly declared.  */
  if (!TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn))
      && DECL_DEFAULTED_IN_CLASS_P (fn))
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
	  auto_diagnostic_group d;
	  error ("explicitly defaulted function %q+D cannot be declared "
		 "%qs because the implicit declaration is not %qs:", fn,
		 DECL_IMMEDIATE_FUNCTION_P (fn) ? "consteval" : "constexpr",
		 "constexpr");
	  explain_implicit_non_constexpr (fn);
	}
      DECL_DECLARED_CONSTEXPR_P (fn) = false;
    }
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
	   && DECL_OVERLOADED_OPERATOR_IS (fn, NOP_EXPR))
    {
      if (copy_fn_p (fn))
	kind = sfk_copy_assignment;
      else if (move_fn_p (fn))
	kind = sfk_move_assignment;
    }
  else if (DECL_OVERLOADED_OPERATOR_CODE_RAW (fn) >= OVL_OP_EQ_EXPR
	   && DECL_OVERLOADED_OPERATOR_CODE_RAW (fn) <= OVL_OP_SPACESHIP_EXPR)
    {
      kind = sfk_comparison;
      if (!early_check_defaulted_comparison (fn))
	return false;
    }

  /* FIXME: We need to check for xobj member functions here to give better
     diagnostics for weird cases where unrelated xobj parameters are given.
     We just want to do better than 'cannot be defaulted'.  */

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
	  suppress_warning (p, OPT_Wunused_parameter);

      if (current_class_type && TYPE_BEING_DEFINED (current_class_type))
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

  type = TYPE_MAIN_VARIANT (type);

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
  if ((sfk == sfk_copy_assignment || sfk == sfk_copy_constructor)
      && cxx_dialect >= cxx11)
    {
      if (classtype_has_move_assign_or_move_ctor_p (type, true))
	DECL_DELETED_FN (fn) = true;
      else if (classtype_has_depr_implicit_copy (type))
	/* The implicit definition of a copy constructor as defaulted is
	   deprecated if the class has a user-declared copy assignment operator
	   or a user-declared destructor. The implicit definition of a copy
	   assignment operator as defaulted is deprecated if the class has a
	   user-declared copy constructor or a user-declared destructor (15.4,
	   15.8).  */
	TREE_DEPRECATED (fn) = true;
    }

  /* Destructors and assignment operators may be virtual.  */
  if (sfk == sfk_destructor
      || sfk == sfk_move_assignment
      || sfk == sfk_copy_assignment)
    check_for_override (fn, type);

  /* Add it to the class  */
  bool added = add_method (type, fn, false);
  gcc_assert (added || errorcount);

  /* Add it to TYPE_FIELDS.  */
  if (sfk == sfk_destructor
      && DECL_VIRTUAL_P (fn))
    /* The ABI requires that a virtual destructor go at the end of the
       vtable.  */
    TYPE_FIELDS (type) = chainon (TYPE_FIELDS (type), fn);
  else
    {
      DECL_CHAIN (fn) = TYPE_FIELDS (type);
      TYPE_FIELDS (type) = fn;
    }
  /* Propagate TYPE_FIELDS.  */
  fixup_type_variants (type);

  maybe_add_class_template_decl_list (type, fn, /*friend_p=*/0);
  if (DECL_MAYBE_IN_CHARGE_CDTOR_P (fn))
    /* Create appropriate clones.  */
    clone_cdtor (fn, /*update_methods=*/true);

  /* Classes, structs or unions TYPE marked with hotness attributes propagate
     the attribute to all methods.  This is typically done in
     check_bases_and_members, but we must also inject them here for deferred
     lazily-declared functions.  */
  maybe_propagate_warmth_attributes (fn, type);

  return fn;
}

/* Given a FUNCTION_DECL FN and a chain LIST, skip as many elements of LIST
   as there are artificial parms in FN.  */

tree
skip_artificial_parms_for (const_tree fn, tree list)
{
  if (DECL_IOBJ_MEMBER_FUNCTION_P (fn))
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

  if (DECL_IOBJ_MEMBER_FUNCTION_P (fn))
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
