/* C++ contracts.

   Copyright (C) 2020-2026 Free Software Foundation, Inc.
   Originally by Jeff Chapman II (jchapman@lock3software.com) for proposed
   C++20 contracts.
   Rewritten for C++26 contracts by:
     Nina Ranns (dinka.ranns@googlemail.com)
     Iain Sandoe (iain@sandoe.co.uk)
     Ville Voutilainen (ville.voutilainen@gmail.com).

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "diagnostic.h"
#include "options.h"
#include "contracts.h"
#include "tree.h"
#include "tree-inline.h"
#include "attribs.h"
#include "tree-iterator.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "intl.h"
#include "cgraph.h"
#include "opts.h"
#include "output.h"

/*  Design notes.

  There are three phases:
    1. Parsing and semantic checks.
       Most of the code for this is in the parser, with helpers provided here.
    2. Emitting contract assertion AST nodes into function bodies.
       This is initiated from "finish_function ()"
    3. Lowering the contract assertion AST nodes to control flow, constant
       data and calls to the violation handler.
       This is initiated from "cp_genericize ()".

  The organisation of the code in this file is intended to follow those three
  phases where possible.

  Contract Assertion State
  ========================

  contract_assert () does not require any special handling and can be
  represented directly by AST inserted in the function body.

  'pre' and 'post' function contract specifiers require most of the special
  handling, since they must be tracked across re-declarations of functions and
  there are contraints on how such specifiers may change in these cases.

  The contracts specification identifies a "first declaration" of any given
  function - which is the first encountered when parsing a given TU.
  Subsequent re-declarations may not add or change the function contract
  specifiers from any introduced on this first declaration.  It is, however,
  permitted to omit specifiers on re-declarations.

  Since the implementation of GCC's (re-)declarations is a destructive merge
  we need to keep some state on the side to determine whether the re-declaration
  rules are met.  In this current design we have chosen not to add another tree
  to each function decl but, instead, keep a map from function decl to contract
  specifier state.  In this state we record the 'first declaration' specifiers
  which are used to validate re-declaration(s) and to report the initial state
  in diagnostics.

  We need (for example) to compare
    pre ( x > 2 ) equal to
    pre ( z > 2 ) when x and z refer to the same function parameter in a
    re-declaration.

  The mechanism used to determine if two contracts are the same is to compare
  the folded trees.  This makes use of current compiler machinery, rather than
  constructing some new AST comparison scheme.  However, it does introduce an
  additional complexity in that we need to defer such comparison until parsing
  is complete - and function contract specifiers in class declarations must be
  deferred parses, since it is also permitted for specifiers to refer to class
  members.

  When we encounter a definition, the parameter names in a function decl are
  re-written to match those of the definition (thus the expected names will
  appear in debug information etc).  At this point, we also need to re-map
  any function parameter names that appear in function contract specifiers
  to agree with those of the definition - although we intend to keep the
  'first declaration' record consistent for diagnostics.

  Since we shared some code from the C++2a contracts implementation, pre and
  post specifiers are represented by chains of attributes, where the payload
  of the attribute is an AST node.  However during the parse, these are not
  inserted into the function bodies, but kept in the decl-keyed state described
  above.  A future improvement planned here is to store the specifiers using a
  tree vec instead of the attribute list.

  Emitting contract AST
  =====================

  When we reach `finish_function ()` and therefore are committed to potentially
  emitting code for an instance, we build a new variant of the function body
  with the pre-condition AST inserted before the user's function body, and the
  post condition AST (if any) linked into the function return.

  Lowering the contract assertion AST
  ===================================

  In all cases (pre, post, contract_assert) the AST node is lowered to control
  flow and (potentially) calls to the violation handler and/or termination.
  This is done during `cp_genericize ()`.  In the current implementation, the
  decision on the control flow is made on the basis of the setting of a command-
  line flag that determines a TU-wide contract evaluation semantic, which has
  the following initial set of behaviours:

    'ignore'	    : contract assertion AST is lowered to 'nothing',
		      i.e. omitted.
    'enforce'	    : contract assertion AST is lowered to a check, if this
		      fails a violation handler is called, followed by
		      std::terminate().
    'quick_enforce' : contract assertion AST is lowered to a check, if this
		      fails, std::terminate () is called.
    'observe'	    : contract assertion AST is lowered to a check, if this
		      fails, a violation handler is called, the code then
		      continues.

  In each case, the "check" might be a simple 'if' (when it is determined that
  the assertion condition does not throw) or the condition evaluation will be
  wrapped in a try-catch block that treats any exception thrown when evaluating
  the check as equivalent to a failed check.  It is noted in the violation data
  object whether a check failed because of an exception raised in evaluation.

  At present, a simple (but potentially space-inefficient) scheme is used to
  store constant data objects that represent the read-only data for the
  violation.  The exact form of this is subject to revision as it represents
  ABI that must be agreed between implementations (as of this point, that
  discussion is not yet concluded).  */

/* Contract matching.  */

bool comparing_contracts;

/* True if the contract is valid.  */

static bool
contract_valid_p (tree contract)
{
  return CONTRACT_CONDITION (contract) != error_mark_node;
}

/* True if the contract specifier is valid.  */

static bool
contract_specifier_valid_p (tree contract)
{
  return contract_valid_p (TREE_VALUE (TREE_VALUE (contract)));
}

/* Compare the contract conditions of OLD_CONTRACT and NEW_CONTRACT.
   Returns false if the conditions are equivalent, and true otherwise.  */

static bool
mismatched_contracts_p (tree old_contract, tree new_contract)
{
  /* Different kinds of contracts do not match.  */
  if (TREE_CODE (old_contract) != TREE_CODE (new_contract))
    {
      auto_diagnostic_group d;
      error_at (EXPR_LOCATION (new_contract),
		"mismatched contract specifier in declaration");
      inform (EXPR_LOCATION (old_contract), "previous contract here");
      return true;
    }

  /* A deferred contract tentatively matches.  */
  if (CONTRACT_CONDITION_DEFERRED_P (new_contract))
    return false;

  /* Compare the conditions of the contracts.  */
  tree t1 = cp_fully_fold_init (CONTRACT_CONDITION (old_contract));
  tree t2 = cp_fully_fold_init (CONTRACT_CONDITION (new_contract));

  /* Compare the contracts. */

  bool saved_comparing_contracts = comparing_contracts;
  comparing_contracts = true;
  bool matching_p = cp_tree_equal (t1, t2);
  comparing_contracts = saved_comparing_contracts;

  if (!matching_p)
    {
      auto_diagnostic_group d;
      error_at (EXPR_LOCATION (CONTRACT_CONDITION (new_contract)),
		"mismatched contract condition in declaration");
      inform (EXPR_LOCATION (CONTRACT_CONDITION (old_contract)),
	      "previous contract here");
      return true;
    }

  return false;
}

/* Compare the contract specifiers of OLDDECL and NEWDECL. Returns true
   if the contracts match, and false if they differ.  */

static bool
match_contract_specifiers (location_t oldloc, tree old_contracts,
			   location_t newloc, tree new_contracts)
{
  /* Contracts only match if they are both specified.  */
  if (!old_contracts || !new_contracts)
    return true;

  /* Compare each contract in turn.  */
  while (old_contracts && new_contracts)
    {
      /* If either contract is ill-formed, skip the rest of the comparison,
	 since we've already diagnosed an error.  */
      if (!contract_specifier_valid_p (new_contracts)
	  || !contract_specifier_valid_p (old_contracts))
	return false;

      if (mismatched_contracts_p (CONTRACT_STATEMENT (old_contracts),
				  CONTRACT_STATEMENT (new_contracts)))
	return false;
      old_contracts = TREE_CHAIN (old_contracts);
      new_contracts = TREE_CHAIN (new_contracts);
    }

  /* If we didn't compare all specifiers, the contracts don't match.  */
  if (old_contracts || new_contracts)
    {
      auto_diagnostic_group d;
      error_at (newloc,
		"declaration has a different number of contracts than "
		"previously declared");
      inform (oldloc,
	      new_contracts
	      ? "previous declaration with fewer contracts here"
	      : "previous declaration with more contracts here");
      return false;
    }

  return true;
}

/* Return true if CONTRACT is checked or assumed under the current build
   configuration. */

static bool
contract_active_p (tree contract)
{
  return get_evaluation_semantic (contract) != CES_IGNORE;
}

/* True if FNDECL has any checked or assumed contracts whose TREE_CODE is
   C.  */

static bool
has_active_contract_condition (tree fndecl, tree_code c)
{
  tree as = get_fn_contract_specifiers (fndecl);
  for (; as != NULL_TREE; as = TREE_CHAIN (as))
    {
      tree contract = TREE_VALUE (TREE_VALUE (as));
      if (TREE_CODE (contract) == c && contract_active_p (contract))
	return true;
    }
  return false;
}

/* True if FNDECL has any checked or assumed preconditions.  */

static bool
has_active_preconditions (tree fndecl)
{
  return has_active_contract_condition (fndecl, PRECONDITION_STMT);
}

/* True if FNDECL has any checked or assumed postconditions.  */

static bool
has_active_postconditions (tree fndecl)
{
  return has_active_contract_condition (fndecl, POSTCONDITION_STMT);
}

/* Return true if any contract in the CONTRACT list is checked or assumed
   under the current build configuration.  */

static bool
contract_any_active_p (tree fndecl)
{
  tree as = get_fn_contract_specifiers (fndecl);
  for (; as; as = TREE_CHAIN (as))
    if (contract_active_p (TREE_VALUE (TREE_VALUE (as))))
      return true;
  return false;
}

/* Return true if any contract in CONTRACTS is not yet parsed.  */

bool
contract_any_deferred_p (tree contracts)
{
  for (; contracts; contracts = TREE_CHAIN (contracts))
    if (CONTRACT_CONDITION_DEFERRED_P (CONTRACT_STATEMENT (contracts)))
      return true;
  return false;
}

/* Returns true if function decl FNDECL has contracts and we need to
   process them for the purposes of either building caller or definition
   contract checks.
   This function does not take into account whether caller or definition
   side checking is enabled. Those checks will be done from the calling
   function which will be able to determine whether it is doing caller
   or definition contract handling.  */

static bool
handle_contracts_p (tree fndecl)
{
  return (flag_contracts
	  && !processing_template_decl
	  && (CONTRACT_HELPER (fndecl) == ldf_contract_none)
	  && contract_any_active_p (fndecl));
}

/* For use with the tree inliner. This preserves non-mapped local variables,
   such as postcondition result variables, during remapping.  */

static tree
retain_decl (tree decl, copy_body_data *)
{
  return decl;
}

/* Lookup a name in std::, or inject it.  */

static tree
lookup_std_type (tree name_id)
{
  tree res_type = lookup_qualified_name
    (std_node, name_id, LOOK_want::TYPE | LOOK_want::HIDDEN_FRIEND);

  if (TREE_CODE (res_type) == TYPE_DECL)
    res_type = TREE_TYPE (res_type);
  else
    {
      push_nested_namespace (std_node);
      res_type = make_class_type (RECORD_TYPE);
      create_implicit_typedef (name_id, res_type);
      DECL_SOURCE_LOCATION (TYPE_NAME (res_type)) = BUILTINS_LOCATION;
      DECL_CONTEXT (TYPE_NAME (res_type)) = current_namespace;
      pushdecl_namespace_level (TYPE_NAME (res_type), /*hidden*/true);
      pop_nested_namespace (std_node);
    }
  return res_type;
}

/* Get constract_assertion_kind of the specified contract. Used when building
  contract_violation object.  */

static contract_assertion_kind
get_contract_assertion_kind (tree contract)
{
  if (CONTRACT_ASSERTION_KIND (contract))
    {
      tree s = CONTRACT_ASSERTION_KIND (contract);
      tree i = (TREE_CODE (s) == INTEGER_CST) ? s
					      : DECL_INITIAL (STRIP_NOPS (s));
      gcc_checking_assert (!type_dependent_expression_p (s) && i);
      return (contract_assertion_kind) tree_to_uhwi (i);
    }

  switch (TREE_CODE (contract))
  {
    case ASSERTION_STMT:	return CAK_ASSERT;
    case PRECONDITION_STMT:	return CAK_PRE;
    case POSTCONDITION_STMT:	return CAK_POST;
    default: break;
  }

  gcc_unreachable ();
}

/* Get contract_evaluation_semantic of the specified contract.  */

contract_evaluation_semantic
get_evaluation_semantic (const_tree contract)
{
  if (CONTRACT_EVALUATION_SEMANTIC (contract))
    {
      tree s = CONTRACT_EVALUATION_SEMANTIC (contract);
      tree i = (TREE_CODE (s) == INTEGER_CST) ? s
					      : DECL_INITIAL (STRIP_NOPS (s));
      gcc_checking_assert (!type_dependent_expression_p (s) && i);
      switch (contract_evaluation_semantic ev =
	      (contract_evaluation_semantic) tree_to_uhwi (i))
	{
	/* This needs to be kept in step with any added semantics.  */
	case CES_IGNORE:
	case CES_OBSERVE:
	case CES_ENFORCE:
	case CES_QUICK:
	  return ev;
	default:
	  break;
	}
    }

  gcc_unreachable ();
}

/* Get location of the last contract in the CONTRACTS tree chain.  */

static location_t
get_contract_end_loc (tree contracts)
{
  tree last = NULL_TREE;
  for (tree a = contracts; a; a = TREE_CHAIN (a))
    last = a;
  gcc_checking_assert (last);
  last = CONTRACT_STATEMENT (last);
  return EXPR_LOCATION (last);
}

struct GTY(()) contract_decl
{
  tree contract_specifiers;
  location_t note_loc;
};

static GTY(()) hash_map<tree, contract_decl> *contract_decl_map;

/* Converts a contract condition to bool and ensures it has a location.  */

tree
finish_contract_condition (cp_expr condition)
{
  if (!condition || error_operand_p (condition))
    return condition;

  /* Ensure we have the condition location saved in case we later need to
     emit a conversion error during template instantiation and wouldn't
     otherwise have it.  This differs from maybe_wrap_with_location in that
     it allows wrappers on EXCEPTIONAL_CLASS_P which includes CONSTRUCTORs.  */
  if (!CAN_HAVE_LOCATION_P (condition)
      && condition.get_location () != UNKNOWN_LOCATION)
    {
      tree_code code
	= (((CONSTANT_CLASS_P (condition) && TREE_CODE (condition) != STRING_CST)
	    || (TREE_CODE (condition) == CONST_DECL && !TREE_STATIC (condition)))
	  ? NON_LVALUE_EXPR : VIEW_CONVERT_EXPR);
      condition = build1_loc (condition.get_location (), code,
			      TREE_TYPE (condition), condition);
      EXPR_LOCATION_WRAPPER_P (condition) = true;
    }

  if (type_dependent_expression_p (condition))
    return condition;

  return condition_conversion (condition);
}

/* Wrap the DECL into VIEW_CONVERT_EXPR representing const qualified version
   of the declaration.  */

tree
view_as_const (tree decl)
{
  if (!contract_const_wrapper_p (decl))
    {
      tree ctype = TREE_TYPE (decl);
      location_t loc =
	  EXPR_P (decl) ? EXPR_LOCATION (decl) : DECL_SOURCE_LOCATION (decl);
      ctype = cp_build_qualified_type (ctype, (cp_type_quals (ctype)
					       | TYPE_QUAL_CONST));
      decl = build1 (VIEW_CONVERT_EXPR, ctype, decl);
      SET_EXPR_LOCATION (decl, loc);
      /* Mark the VCE as contract const wrapper.  */
      CONST_WRAPPER_P (decl) = true;
    }
  return decl;
}

/* Constify access to DECL from within the contract condition.  */

tree
constify_contract_access (tree decl)
{
  /* We check if we have a variable, a parameter, a variable of reference type,
   * or a parameter of reference type
   */
  if (!TREE_READONLY (decl)
      && (VAR_P (decl)
	  || (TREE_CODE (decl) == PARM_DECL)
	  || (REFERENCE_REF_P (decl)
	      && (VAR_P (TREE_OPERAND (decl, 0))
		  || (TREE_CODE (TREE_OPERAND (decl, 0)) == PARM_DECL)
		  || (TREE_CODE (TREE_OPERAND (decl, 0))
		      == TEMPLATE_PARM_INDEX)))))
    decl = view_as_const (decl);

  return decl;
}

/* Indicate that PARM_DECL DECL is ODR used in a postcondition.  */

static void
set_parm_used_in_post (tree decl, bool constify = true)
{
  gcc_checking_assert (TREE_CODE (decl) == PARM_DECL);
  DECL_LANG_FLAG_4 (decl) = constify;
}

/* Test if PARM_DECL is ODR used in a postcondition.  */

static bool
parm_used_in_post_p (const_tree decl)
{
  /* Check if this parameter is odr used within a function's postcondition  */
  return ((TREE_CODE (decl) == PARM_DECL) && DECL_LANG_FLAG_4 (decl));
}

/* If declaration DECL is a PARM_DECL and it appears in a postcondition, then
   check that it is not a non-const by-value param. LOCATION is where the
   expression was found and is used for diagnostic purposes.  */

void
check_param_in_postcondition (tree decl, location_t location)
{
  if (processing_postcondition
      && TREE_CODE (decl) == PARM_DECL
      /* TREE_CODE (decl) == PARM_DECL only holds for non-reference
	 parameters.  */
      && !cp_unevaluated_operand
      /* Return value parameter has DECL_ARTIFICIAL flag set. The flag
	 presence of the flag should be sufficient to distinguish the
	 return value parameter in this context.  */
      && !(DECL_ARTIFICIAL (decl)))
    {
      set_parm_used_in_post (decl);

      if (!dependent_type_p (TREE_TYPE (decl))
	  && !CP_TYPE_CONST_P (TREE_TYPE (decl)))
	{
	  error_at (location,
		    "a value parameter used in a postcondition must be const");
	  inform (DECL_SOURCE_LOCATION (decl), "parameter declared here");
	}
    }
}

/* Check if parameters used in postconditions are const qualified on
   a redeclaration that does not specify contracts or on an instantiation
   of a function template.  */

void
check_postconditions_in_redecl (tree olddecl, tree newdecl)
{
  tree contract_spec = get_fn_contract_specifiers (olddecl);
  if (!contract_spec)
    return;

  tree t1 = FUNCTION_FIRST_USER_PARM (olddecl);
  tree t2 = FUNCTION_FIRST_USER_PARM (newdecl);

  for (; t1 && t1 != void_list_node;
  t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    {
      if (parm_used_in_post_p (t1))
	{
	  set_parm_used_in_post (t2);
	  if (!dependent_type_p (TREE_TYPE (t2))
	      && !CP_TYPE_CONST_P (TREE_TYPE (t2))
	      && !TREE_READONLY (t2))
	    {
	      error_at (DECL_SOURCE_LOCATION (t2),
	      "value parameter %qE used in a postcondition must be const", t2);
	      inform (DECL_SOURCE_LOCATION (olddecl),
	      "previous declaration here");
	    }
	}
    }
}

/* Map from FUNCTION_DECL to a FUNCTION_DECL for either the PRE_FN or POST_FN.
   These are used to parse contract conditions and are called inside the body
   of the guarded function.  */
static GTY(()) hash_map<tree, tree> *decl_pre_fn;
static GTY(()) hash_map<tree, tree> *decl_post_fn;

/* Given a pre or post function decl (for an outlined check function) return
   the decl for the function for which the outlined checks are being
   performed.  */
static GTY(()) hash_map<tree, tree> *orig_from_outlined;

/* Makes PRE the precondition function for FNDECL.  */

static void
set_precondition_function (tree fndecl, tree pre)
{
  gcc_assert (pre);
  hash_map_maybe_create<hm_ggc> (decl_pre_fn);
  gcc_checking_assert (!decl_pre_fn->get (fndecl));
  decl_pre_fn->put (fndecl, pre);

  hash_map_maybe_create<hm_ggc> (orig_from_outlined);
  gcc_checking_assert (!orig_from_outlined->get (pre));
  orig_from_outlined->put (pre, fndecl);
}

/* Makes POST the postcondition function for FNDECL.  */

static void
set_postcondition_function (tree fndecl, tree post)
{
  gcc_checking_assert (post);
  hash_map_maybe_create<hm_ggc> (decl_post_fn);
  gcc_checking_assert (!decl_post_fn->get (fndecl));
  decl_post_fn->put (fndecl, post);

  hash_map_maybe_create<hm_ggc> (orig_from_outlined);
  gcc_checking_assert (!orig_from_outlined->get (post));
  orig_from_outlined->put (post, fndecl);
}

/* For a given pre or post condition function, find the checked function.  */
tree
get_orig_for_outlined (tree fndecl)
{
  gcc_checking_assert (fndecl);
  tree *result = hash_map_safe_get (orig_from_outlined, fndecl);
  return result ? *result : NULL_TREE ;
}

/* For a given function OLD_FN set suitable names for NEW_FN (which is an
   outlined contract check) usually by appending '.pre' or '.post'.

   For functions with special meaning names (i.e. main and cdtors) we need to
   make special provisions and therefore handle all the contracts function
   name changes here, rather than requiring a separate update to mangle.cc.

   PRE specifies if we need an identifier for a pre or post contract check.  */

static void
contracts_fixup_names (tree new_fn, tree old_fn, bool pre, bool wrapper)
{
  bool cdtor = DECL_CXX_CONSTRUCTOR_P (old_fn)
	       || DECL_CXX_DESTRUCTOR_P (old_fn);
  const char *fname = IDENTIFIER_POINTER (DECL_NAME (old_fn));
  const char *append = wrapper ? "contract_wrapper"
			       : (pre ? "pre" : "post");
  size_t len = strlen (fname);
  /* Cdtor names have a space at the end.  We need to remove that space
     when forming the new identifier.  */
  char *nn = xasprintf ("%.*s%s%s",
			cdtor ? (int)len-1 : int(len),
			fname,
			JOIN_STR,
			append);
  DECL_NAME (new_fn) = get_identifier (nn);
  free (nn);

  /* Now do the mangled version.  */
  fname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (old_fn));
  nn = xasprintf ("%s%s%s", fname, JOIN_STR, append);
  SET_DECL_ASSEMBLER_NAME (new_fn, get_identifier (nn));
  free (nn);
}

/* Build a declaration for the pre- or postcondition of a guarded FNDECL.  */

static tree
build_contract_condition_function (tree fndecl, bool pre)
{
  if (error_operand_p (fndecl))
    return error_mark_node;

  /* Start the copy.  */
  tree fn = copy_decl (fndecl);

  /* Don't propagate declaration attributes to the checking function,
     including the original contracts.  */
  DECL_ATTRIBUTES (fn) = NULL_TREE;

  /* If requested, disable optimisation of checking functions; this can, in
     some cases, prevent UB from eliding the checks themselves.  */
  if (flag_contract_disable_optimized_checks)
    DECL_ATTRIBUTES (fn)
      = tree_cons (get_identifier ("optimize"),
		   build_tree_list (NULL_TREE, build_string (3, "-O0")),
		   NULL_TREE);

  /* Now parse and add any internal representation of these attrs to the
     decl.  */
  if (DECL_ATTRIBUTES (fn))
    cplus_decl_attributes (&fn, DECL_ATTRIBUTES (fn), 0);

  /* A possible later optimization may delete unused args to prevent extra arg
     passing.  */
  /* Handle the args list.  */
  tree arg_types = NULL_TREE;
  tree *last = &arg_types;
  for (tree arg_type = TYPE_ARG_TYPES (TREE_TYPE (fn));
      arg_type && arg_type != void_list_node;
      arg_type = TREE_CHAIN (arg_type))
    {
      if (DECL_IOBJ_MEMBER_FUNCTION_P (fndecl)
	  && TYPE_ARG_TYPES (TREE_TYPE (fn)) == arg_type)
      continue;
      *last = build_tree_list (TREE_PURPOSE (arg_type), TREE_VALUE (arg_type));
      last = &TREE_CHAIN (*last);
    }

  /* Copy the function parameters, if present.  Disable warnings for them.  */
  DECL_ARGUMENTS (fn) = NULL_TREE;
  if (DECL_ARGUMENTS (fndecl))
    {
      tree *last_a = &DECL_ARGUMENTS (fn);
      for (tree p = DECL_ARGUMENTS (fndecl); p; p = TREE_CHAIN (p))
	{
	  *last_a = copy_decl (p);
	  suppress_warning (*last_a);
	  DECL_CONTEXT (*last_a) = fn;
	  last_a = &TREE_CHAIN (*last_a);
	}
    }

  tree orig_fn_value_type = TREE_TYPE (TREE_TYPE (fn));
  if (!pre && !VOID_TYPE_P (orig_fn_value_type))
    {
      /* For post contracts that deal with a non-void function, append a
	 parameter to pass the return value.  */
      tree name = get_identifier ("__r");
      tree parm = build_lang_decl (PARM_DECL, name, orig_fn_value_type);
      DECL_CONTEXT (parm) = fn;
      DECL_ARTIFICIAL (parm) = true;
      suppress_warning (parm);
      DECL_ARGUMENTS (fn) = chainon (DECL_ARGUMENTS (fn), parm);
      *last = build_tree_list (NULL_TREE, orig_fn_value_type);
      last = &TREE_CHAIN (*last);
    }

  *last = void_list_node;

  tree adjusted_type = NULL_TREE;

  /* The handlers are void fns.  */
  if (DECL_IOBJ_MEMBER_FUNCTION_P (fndecl))
    adjusted_type = build_method_type_directly (DECL_CONTEXT (fndecl),
						void_type_node,
						arg_types);
  else
    adjusted_type = build_function_type (void_type_node, arg_types);

  /* If the original function is noexcept, build a noexcept function.  */
  if (flag_exceptions && type_noexcept_p (TREE_TYPE (fndecl)))
    adjusted_type = build_exception_variant (adjusted_type, noexcept_true_spec);

  TREE_TYPE (fn) = adjusted_type;
  DECL_RESULT (fn) = NULL_TREE; /* Let the start function code fill it in.  */

  /* The contract check functions are never a cdtor, nor virtual.  */
  DECL_CXX_DESTRUCTOR_P (fn) = DECL_CXX_CONSTRUCTOR_P (fn) = 0;
  DECL_VIRTUAL_P (fn) = false;

  /* Append .pre / .post to a usable name for the original function.  */
  contracts_fixup_names (fn, fndecl, pre, /*wrapper*/false);

  DECL_INITIAL (fn) = NULL_TREE;
  CONTRACT_HELPER (fn) = pre ? ldf_contract_pre : ldf_contract_post;
  /* We might have a pre/post for a wrapper.  */
  DECL_CONTRACT_WRAPPER (fn) = DECL_CONTRACT_WRAPPER (fndecl);

  /* Make these functions internal if we can, i.e. if the guarded function is
     not vague linkage, or if we can put them in a comdat group with the
     guarded function.  */
  if (!DECL_WEAK (fndecl) || HAVE_COMDAT_GROUP)
    {
      TREE_PUBLIC (fn) = false;
      DECL_EXTERNAL (fn) = false;
      DECL_WEAK (fn) = false;
      DECL_COMDAT (fn) = false;

      /* We may not have set the comdat group on the guarded function yet.
	 If we haven't, we'll add this to the same group in comdat_linkage
	 later.  Otherwise, add it to the same comdat group now.  */
      if (DECL_ONE_ONLY (fndecl))
	{
	  symtab_node *n = symtab_node::get (fndecl);
	  cgraph_node::get_create (fn)->add_to_same_comdat_group (n);
	}

    }

  DECL_INTERFACE_KNOWN (fn) = true;
  DECL_ARTIFICIAL (fn) = true;
  suppress_warning (fn);

  return fn;
}

/* Build the precondition checking function for FNDECL.  */

static tree
build_precondition_function (tree fndecl)
{
  if (!has_active_preconditions (fndecl))
    return NULL_TREE;

  return build_contract_condition_function (fndecl, /*pre=*/true);
}

/* Build the postcondition checking function for FNDECL.  If the return
   type is undeduced, don't build the function yet.  We do that in
   apply_deduced_return_type.  */

static tree
build_postcondition_function (tree fndecl)
{
  if (!has_active_postconditions (fndecl))
    return NULL_TREE;

  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  if (is_auto (type))
    return NULL_TREE;

  return build_contract_condition_function (fndecl, /*pre=*/false);
}

/* If we're outlining the contract, build the functions to do the
   precondition and postcondition checks, and associate them with
   the function decl FNDECL.
 */

static void
build_contract_function_decls (tree fndecl)
{
  /* Build the pre/post functions (or not).  */
  if (!get_precondition_function (fndecl))
    if (tree pre = build_precondition_function (fndecl))
      set_precondition_function (fndecl, pre);

  if (!get_postcondition_function (fndecl))
    if (tree post = build_postcondition_function (fndecl))
      set_postcondition_function (fndecl, post);
}

/* Map from FUNCTION_DECL to a FUNCTION_DECL for contract wrapper.  */

static GTY(()) hash_map<tree, tree> *decl_wrapper_fn = nullptr;

/* Map from the function decl of a wrapper to the function that it wraps.  */

static GTY(()) hash_map<tree, tree> *decl_for_wrapper = nullptr;

/* Makes wrapper the precondition function for FNDECL.  */

static void
set_contract_wrapper_function (tree fndecl, tree wrapper)
{
  gcc_checking_assert (wrapper && fndecl);
  hash_map_maybe_create<hm_ggc> (decl_wrapper_fn);
  gcc_checking_assert (decl_wrapper_fn && !decl_wrapper_fn->get (fndecl));
  decl_wrapper_fn->put (fndecl, wrapper);

  /* We need to know the wrapped function when composing the diagnostic.  */
  hash_map_maybe_create<hm_ggc> (decl_for_wrapper);
  gcc_checking_assert (decl_for_wrapper && !decl_for_wrapper->get (wrapper));
  decl_for_wrapper->put (wrapper, fndecl);
}

/* Returns the wrapper function decl for FNDECL, or null if not set.  */

static tree
get_contract_wrapper_function (tree fndecl)
{
  gcc_checking_assert (fndecl);
  tree *result = hash_map_safe_get (decl_wrapper_fn, fndecl);
  return result ? *result : NULL_TREE;
}

/* Given a wrapper function WRAPPER, find the original function decl.  */

static tree
get_orig_func_for_wrapper (tree wrapper)
{
  gcc_checking_assert (wrapper);
  tree *result = hash_map_safe_get (decl_for_wrapper, wrapper);
  return result ? *result : NULL_TREE;
}

/* Build a declaration for the contract wrapper of a caller FNDECL.
   We're making a caller side contract check wrapper. For caller side contract
   checks, postconditions are only checked if check_post is true.
   Defer the attachment of the contracts to this function until the callee
   is non-dependent, or we get cases where the conditions can be non-dependent
   but still need tsubst-ing.  */

static tree
build_contract_wrapper_function (tree fndecl)
{
  if (error_operand_p (fndecl))
    return error_mark_node;

  /* We should not be trying to build wrappers for templates or functions that
     are still dependent.  */
  gcc_checking_assert (!processing_template_decl
		       && !TYPE_DEPENDENT_P (TREE_TYPE (fndecl)));

  location_t loc = DECL_SOURCE_LOCATION (fndecl);

  /* Fill in the names later.  */
  tree wrapdecl
    = build_lang_decl_loc (loc, FUNCTION_DECL, NULL_TREE, TREE_TYPE (fndecl));

  /* Put the wrapper in the same context as the callee.  */
  DECL_CONTEXT (wrapdecl) = DECL_CONTEXT (fndecl);

  /* This declaration is a contract wrapper function.  */
  DECL_CONTRACT_WRAPPER (wrapdecl) = true;

  contracts_fixup_names (wrapdecl, fndecl, /*pre*/false, /*wrapper*/true);

  DECL_SOURCE_LOCATION (wrapdecl) = loc;
  /* The declaration was implicitly generated by the compiler.  */
  DECL_ARTIFICIAL (wrapdecl) = true;
  /* Declaration, no definition yet.  */
  DECL_INITIAL (wrapdecl) = NULL_TREE;

  /* Let the start function code fill in the result decl.  */
  DECL_RESULT (wrapdecl) = NULL_TREE;

  /* Copy the function parameters, if present.  Suppress (e.g. unused)
     warnings on them.  */
  DECL_ARGUMENTS (wrapdecl) = NULL_TREE;
  if (tree p = DECL_ARGUMENTS (fndecl))
    {
      tree *last_a = &DECL_ARGUMENTS (wrapdecl);
      for (; p; p = TREE_CHAIN (p))
	{
	  *last_a = copy_decl (p);
	  suppress_warning (*last_a);
	  DECL_CONTEXT (*last_a) = wrapdecl;
	  last_a = &TREE_CHAIN (*last_a);
	}
    }

  /* Copy selected attributes from the original function.  */
  TREE_USED (wrapdecl) = TREE_USED (fndecl);

  /* Copy any alignment added.  */
  if (DECL_ALIGN (fndecl))
    SET_DECL_ALIGN (wrapdecl, DECL_ALIGN (fndecl));
  DECL_USER_ALIGN (wrapdecl) = DECL_USER_ALIGN (fndecl);

  /* Make this function internal.  */
  TREE_PUBLIC (wrapdecl) = false;
  DECL_EXTERNAL (wrapdecl) = false;
  DECL_WEAK (wrapdecl) = false;

  /* We know this is an internal function.  */
  DECL_INTERFACE_KNOWN (wrapdecl) = true;
  return wrapdecl;
}

static tree
get_or_create_contract_wrapper_function (tree fndecl)
{
  tree wrapdecl = get_contract_wrapper_function (fndecl);
  if (!wrapdecl)
    {
      wrapdecl = build_contract_wrapper_function (fndecl);
      set_contract_wrapper_function (fndecl, wrapdecl);
    }
  return wrapdecl;
}

void
start_function_contracts (tree fndecl)
{
  if (error_operand_p (fndecl))
    return;

  if (!handle_contracts_p (fndecl))
    return;

  /* If this is not a client side check and definition side checks are
     disabled, do nothing.  */
  if (!flag_contracts_definition_check
      && !DECL_CONTRACT_WRAPPER (fndecl))
    return;

  /* Check that the postcondition result name, if any, does not shadow a
     function parameter.  */
  for (tree ca = get_fn_contract_specifiers (fndecl); ca; ca = TREE_CHAIN (ca))
    if (POSTCONDITION_P (CONTRACT_STATEMENT (ca)))
      if (tree id = POSTCONDITION_IDENTIFIER (CONTRACT_STATEMENT (ca)))
	{
	  if (id == error_mark_node)
	    {
	      CONTRACT_CONDITION (CONTRACT_STATEMENT (ca)) = error_mark_node;
	      continue;
	    }
	  tree r_name = tree_strip_any_location_wrapper (id);
	  if (TREE_CODE (id) == PARM_DECL)
	    r_name = DECL_NAME (id);
	  gcc_checking_assert (r_name && TREE_CODE (r_name) == IDENTIFIER_NODE);
	  tree seen = lookup_name (r_name);
	  if (seen
	      && TREE_CODE (seen) == PARM_DECL
	      && DECL_CONTEXT (seen) == fndecl)
	    {
		auto_diagnostic_group d;
		location_t id_l = location_wrapper_p (id)
				  ? EXPR_LOCATION (id)
				  : DECL_SOURCE_LOCATION (id);
		location_t co_l = EXPR_LOCATION (CONTRACT_STATEMENT (ca));
		if (id_l != UNKNOWN_LOCATION)
		  co_l = make_location (id_l, co_l, co_l);
		error_at (co_l, "contract postcondition result name shadows a"
			  " function parameter");
		inform (DECL_SOURCE_LOCATION (seen),
			"parameter declared here");
		POSTCONDITION_IDENTIFIER (CONTRACT_STATEMENT (ca))
		  = error_mark_node;
		CONTRACT_CONDITION (CONTRACT_STATEMENT (ca)) = error_mark_node;
	    }
	}

  /* If we are expanding contract assertions inline then no need to declare
     the outline function decls.  */
  if (!flag_contract_checks_outlined)
    return;

  /* Contracts may have just been added without a chance to parse them, though
     we still need the PRE_FN available to generate a call to it.  */
  /* Do we already have declarations generated ? */
  if (!DECL_PRE_FN (fndecl) && !DECL_POST_FN (fndecl))
    build_contract_function_decls (fndecl);
}

void
maybe_update_postconditions (tree fndecl)
{
  /* Update any postconditions and the postcondition checking function
     as needed.  If there are postconditions, we'll use those to rewrite
     return statements to check postconditions.  */
  if (has_active_postconditions (fndecl))
    {
      rebuild_postconditions (fndecl);
      tree post = build_postcondition_function (fndecl);
      set_postcondition_function (fndecl, post);
    }
}

/* Build and return an argument list containing all the parameters of the
   (presumably guarded) function decl FNDECL.  This can be used to forward
   all of FNDECL arguments to a function taking the same list of arguments
   -- namely the unchecked form of FNDECL.

   We use CALL_FROM_THUNK_P instead of forward_parm for forwarding
   semantics.  */

static vec<tree, va_gc> *
build_arg_list (tree fndecl)
{
  vec<tree, va_gc> *args = make_tree_vector ();
  for (tree t = DECL_ARGUMENTS (fndecl); t; t = DECL_CHAIN (t))
    vec_safe_push (args, t);
  return args;
}

/* Build and return a thunk like call to FUNC from CALLER using the supplied
   arguments.  The call is like a thunk call in the fact that we do not
   want to create additional copies of the arguments.  We can not simply reuse
   the thunk machinery as it does more than we want.  More specifically, we
   don't want to mark the calling function as `DECL_THUNK_P` for this
   particular purpose, we only want the special treatment for the parameters
   of the call we are about to generate.  We temporarily mark the calling
   function as DECL_THUNK_P so build_call_a does the right thing.  */

static tree
build_thunk_like_call (tree func, int n, tree *argarray)
{
  bool old_decl_thunk_p = DECL_THUNK_P (current_function_decl);
  LANG_DECL_FN_CHECK (current_function_decl)->thunk_p  = true;

  tree call = build_call_a (func, n, argarray);

  /* Revert the `DECL_THUNK_P` flag.  */
  LANG_DECL_FN_CHECK (current_function_decl)->thunk_p = old_decl_thunk_p;

  /* Mark the call as a thunk call to allow for correct gimplification
   of the arguments.  */
  CALL_FROM_THUNK_P (call) = true;

  return call;
}

/* If we have a precondition function and it's valid, call it.  */

static void
add_pre_condition_fn_call (tree fndecl)
{
  /* If we're starting a guarded function with valid contracts, we need to
     insert a call to the pre function.  */
  gcc_checking_assert (DECL_PRE_FN (fndecl)
		       && DECL_PRE_FN (fndecl) != error_mark_node);

  releasing_vec args = build_arg_list (fndecl);
  tree call = build_thunk_like_call (DECL_PRE_FN (fndecl),
				     args->length (), args->address ());

  finish_expr_stmt (call);
}

/* Returns the parameter corresponding to the return value of a guarded
   function FNDECL.  Returns NULL_TREE if FNDECL has no postconditions or
   is void.  */

static tree
get_postcondition_result_parameter (tree fndecl)
{
  if (!fndecl || fndecl == error_mark_node)
    return NULL_TREE;

  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fndecl))))
    return NULL_TREE;

  tree post = DECL_POST_FN (fndecl);
  if (!post || post == error_mark_node)
    return NULL_TREE;

  /* The last param is the return value.  */
  return tree_last (DECL_ARGUMENTS (post));
}

/* Build and add a call to the post-condition checking function, when that
   is in use.  */

static void
add_post_condition_fn_call (tree fndecl)
{
  gcc_checking_assert (DECL_POST_FN (fndecl)
		       && DECL_POST_FN (fndecl) != error_mark_node);

  releasing_vec args = build_arg_list (fndecl);
  if (get_postcondition_result_parameter (fndecl))
    vec_safe_push (args, DECL_RESULT (fndecl));
  tree call = build_thunk_like_call (DECL_POST_FN (fndecl),
				     args->length (), args->address ());
  finish_expr_stmt (call);
}

/* Copy (possibly a sub-set of) contracts from CONTRACTS on FNDECL.  */

static tree
copy_contracts_list (tree contracts, tree fndecl,
		     contract_match_kind remap_kind = cmk_all)
{
  tree last = NULL_TREE, new_contracts = NULL_TREE;
  for (; contracts; contracts = TREE_CHAIN (contracts))
    {
      if ((remap_kind == cmk_pre
	   && (TREE_CODE (CONTRACT_STATEMENT (contracts))
	       == POSTCONDITION_STMT))
	  || (remap_kind == cmk_post
	      && (TREE_CODE (CONTRACT_STATEMENT (contracts))
		  == PRECONDITION_STMT)))
	continue;

      tree c = copy_node (contracts);
      TREE_VALUE (c) = build_tree_list (TREE_PURPOSE (TREE_VALUE (c)),
					copy_node (CONTRACT_STATEMENT (c)));

      copy_body_data id;
      hash_map<tree, tree> decl_map;

      memset (&id, 0, sizeof (id));

      id.src_fn = fndecl;
      id.dst_fn = fndecl;
      id.src_cfun = DECL_STRUCT_FUNCTION (fndecl);
      id.decl_map = &decl_map;

      id.copy_decl = retain_decl;

      id.transform_call_graph_edges = CB_CGE_DUPLICATE;
      id.transform_new_cfg = false;
      id.transform_return_to_modify = false;
      id.transform_parameter = true;

      /* Make sure not to unshare trees behind the front-end's back
	 since front-end specific mechanisms may rely on sharing.  */
      id.regimplify = false;
      id.do_not_unshare = true;
      id.do_not_fold = true;

      /* We're not inside any EH region.  */
      id.eh_lp_nr = 0;
      walk_tree (&CONTRACT_CONDITION (CONTRACT_STATEMENT (c)),
				      copy_tree_body_r, &id, NULL);


      CONTRACT_COMMENT (CONTRACT_STATEMENT (c))
	= copy_node (CONTRACT_COMMENT (CONTRACT_STATEMENT (c)));

      chainon (last, c);
      last = c;
      if (!new_contracts)
	new_contracts = c;
    }
  return new_contracts;
}

/* Returns a copy of FNDECL contracts. This is used when emiting a contract.
 If we were to emit the original contract tree, any folding of the contract
 condition would affect the original contract too. The original contract
 tree needs to be preserved in case it is used to apply to a different
 function (for inheritance or wrapping reasons). */

static tree
copy_contracts (tree fndecl, contract_match_kind remap_kind = cmk_all)
{
  tree contracts = get_fn_contract_specifiers (fndecl);
  return copy_contracts_list (contracts, fndecl, remap_kind);
}

/* Add the contract statement CONTRACT to the current block if valid.  */

static bool
emit_contract_statement (tree contract)
{
  /* Only add valid contracts.  */
  if (contract == error_mark_node
      || CONTRACT_CONDITION (contract) == error_mark_node)
    return false;

  if (get_evaluation_semantic (contract) == CES_INVALID)
    return false;

  add_stmt (contract);
  return true;
}

/* Generate the statement for the given contract by adding the contract
   statement to the current block. Returns the next contract in the chain.  */

static tree
emit_contract (tree contract)
{
  gcc_assert (TREE_CODE (contract) == TREE_LIST);

  emit_contract_statement (CONTRACT_STATEMENT (contract));

  return TREE_CHAIN (contract);
}

/* Add a call or a direct evaluation of the pre checks.  */

static void
apply_preconditions (tree fndecl)
{
  if (flag_contract_checks_outlined)
    add_pre_condition_fn_call (fndecl);
  else
  {
    tree contract_copy = copy_contracts (fndecl, cmk_pre);
    for (; contract_copy; contract_copy = TREE_CHAIN (contract_copy))
      emit_contract (contract_copy);
  }
}

/* Add a call or a direct evaluation of the post checks.  */

static void
apply_postconditions (tree fndecl)
{
  if (flag_contract_checks_outlined)
    add_post_condition_fn_call (fndecl);
  else
    {
      tree contract_copy = copy_contracts (fndecl, cmk_post);
      for (; contract_copy; contract_copy = TREE_CHAIN (contract_copy))
	emit_contract (contract_copy);
    }
}

/* Add contract handling to the function in FNDECL.

   When we have only pre-conditions, this simply prepends a call (or a direct
   evaluation, for cdtors) to the existing function body.

   When we have post conditions we build a try-finally block.
   If the function might throw then the handler in the try-finally is an
   EH_ELSE expression, where the post condition check is applied to the
   non-exceptional path, and an empty statement is added to the EH path.  If
   the function has a non-throwing eh spec, then the handler is simply the
   post-condition checker.  */

void
maybe_apply_function_contracts (tree fndecl)
{
  if (!handle_contracts_p (fndecl))
    /* We did nothing and the original function body statement list will be
       popped by our caller.  */
    return;

  /* If this is not a client side check and definition side checks are
     disabled, do nothing.  */
  if (!flag_contracts_definition_check
      && !DECL_CONTRACT_WRAPPER (fndecl))
    return;

  bool do_pre = has_active_preconditions (fndecl);
  bool do_post = has_active_postconditions (fndecl);
  /* We should not have reached here with nothing to do... */
  gcc_checking_assert (do_pre || do_post);

  /* If the function is noexcept, the user's written body will be wrapped in a
     MUST_NOT_THROW expression.  In that case we leave the MUST_NOT_THROW in
     place and do our replacement inside it.  */
  tree fnbody;
  if (TYPE_NOEXCEPT_P (TREE_TYPE (fndecl)))
    {
      tree m_n_t_expr = expr_first (DECL_SAVED_TREE (fndecl));
      gcc_checking_assert (TREE_CODE (m_n_t_expr) == MUST_NOT_THROW_EXPR);
      fnbody = TREE_OPERAND (m_n_t_expr, 0);
      TREE_OPERAND (m_n_t_expr, 0) = push_stmt_list ();
    }
  else
    {
      fnbody = DECL_SAVED_TREE (fndecl);
      DECL_SAVED_TREE (fndecl) = push_stmt_list ();
    }

  /* Now add the pre and post conditions to the existing function body.
     This copies the approach used for function try blocks.  */
  tree compound_stmt = begin_compound_stmt (0);
  current_binding_level->artificial = true;

  /* Do not add locations for the synthesised code.  */
  location_t loc = UNKNOWN_LOCATION;

  /* For other cases, we call a function to process the check.  */

  /* If we have a pre, but not a post, then just emit that and we are done.  */
  if (!do_post)
    {
      apply_preconditions (fndecl);
      add_stmt (fnbody);
      finish_compound_stmt (compound_stmt);
      return;
    }

  if (do_pre)
    /* Add a precondition call, if we have one. */
    apply_preconditions (fndecl);
  tree try_fin = build_stmt (loc, TRY_FINALLY_EXPR, fnbody, NULL_TREE);
  add_stmt (try_fin);
  TREE_OPERAND (try_fin, 1) = push_stmt_list ();
  /* If we have exceptions, and a function that might throw, then add
     an EH_ELSE clause that allows the exception to propagate upwards
     without encountering the post-condition checks.  */
  if (flag_exceptions && !type_noexcept_p (TREE_TYPE (fndecl)))
    {
      tree eh_else = build_stmt (loc, EH_ELSE_EXPR, NULL_TREE, NULL_TREE);
      add_stmt (eh_else);
      TREE_OPERAND (eh_else, 0) = push_stmt_list ();
      apply_postconditions (fndecl);
      TREE_OPERAND (eh_else, 0) = pop_stmt_list (TREE_OPERAND (eh_else, 0));
      TREE_OPERAND (eh_else, 1) = void_node;
    }
  else
    apply_postconditions (fndecl);
  TREE_OPERAND (try_fin, 1) = pop_stmt_list (TREE_OPERAND (try_fin, 1));
  finish_compound_stmt (compound_stmt);
  /* The DECL_SAVED_TREE stmt list will be popped by our caller.  */
}

/* Rewrite the condition of contract in place, so that references to SRC's
   parameters are updated to refer to DST's parameters. The postcondition
   result variable is left unchanged.

   When declarations are merged, we sometimes need to update contracts to
   refer to new parameters.

   If DUPLICATE_P is true, this is called by duplicate_decls to rewrite
   contracts in terms of a new set of parameters.  This also preserves the
   references to postcondition results, which are not replaced during
   merging.  */

static void
remap_contract (tree src, tree dst, tree contract, bool duplicate_p)
{
  copy_body_data id;
  hash_map<tree, tree> decl_map;

  memset (&id, 0, sizeof (id));
  id.src_fn = src;
  id.dst_fn = dst;
  id.src_cfun = DECL_STRUCT_FUNCTION (src);
  id.decl_map = &decl_map;

  /* If we're merging contracts, don't copy local variables.  */
  id.copy_decl = duplicate_p ? retain_decl : copy_decl_no_change;

  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = false;
  id.transform_return_to_modify = false;
  id.transform_parameter = true;

  /* Make sure not to unshare trees behind the front-end's back
     since front-end specific mechanisms may rely on sharing.  */
  id.regimplify = false;
  id.do_not_unshare = true;
  id.do_not_fold = true;

  /* We're not inside any EH region.  */
  id.eh_lp_nr = 0;

  bool do_remap = false;

  /* Insert parameter remappings.  */
  gcc_checking_assert (TREE_CODE (src) == FUNCTION_DECL);
  gcc_checking_assert (TREE_CODE (dst) == FUNCTION_DECL);

  int src_num_artificial_args = num_artificial_parms_for (src);
  int dst_num_artificial_args = num_artificial_parms_for (dst);

  for (tree sp = DECL_ARGUMENTS (src), dp = DECL_ARGUMENTS (dst);
       sp || dp;
       sp = DECL_CHAIN (sp), dp = DECL_CHAIN (dp))
    {
      if (!sp && dp
	  && TREE_CODE (contract) == POSTCONDITION_STMT
	  && DECL_CHAIN (dp) == NULL_TREE)
	{
	  gcc_assert (!duplicate_p);
	  if (tree result = POSTCONDITION_IDENTIFIER (contract))
	    {
	      gcc_assert (DECL_P (result));
	      insert_decl_map (&id, result, dp);
	      do_remap = true;
	    }
	  break;
	}
      gcc_assert (sp && dp);

      if (sp == dp)
	continue;

      insert_decl_map (&id, sp, dp);
      do_remap = true;

      /* First artificial arg is *this. We want to remap that.  However, we
	 want to skip _in_charge param and __vtt_parm.  Do so now.  */
      if (src_num_artificial_args > 0)
	{
	  while (--src_num_artificial_args,src_num_artificial_args > 0)
	    sp = DECL_CHAIN (sp);
	}
      if (dst_num_artificial_args > 0)
	{
	  while (--dst_num_artificial_args,dst_num_artificial_args > 0)
	    dp = DECL_CHAIN (dp);
	}
    }

  if (!do_remap)
    return;

  walk_tree (&CONTRACT_CONDITION (contract), copy_tree_body_r, &id, NULL);
}

/* Returns a copy of SOURCE contracts where any references to SOURCE's
   PARM_DECLs have been rewritten to the corresponding PARM_DECL in DEST.  */

tree
copy_and_remap_contracts (tree dest, tree source,
			  contract_match_kind remap_kind)
{
  tree last = NULL_TREE, contracts_copy= NULL_TREE;
  tree contracts = get_fn_contract_specifiers (source);
  for (; contracts; contracts = TREE_CHAIN (contracts))
    {
      if ((remap_kind == cmk_pre
	   && (TREE_CODE (CONTRACT_STATEMENT (contracts))
	       == POSTCONDITION_STMT))
	  || (remap_kind == cmk_post
	      && (TREE_CODE (CONTRACT_STATEMENT (contracts))
		  == PRECONDITION_STMT)))
	continue;

      /* The first part is copying of the legacy attribute layout - eventually
	 this will go away.  */
      tree c = copy_node (contracts);
      TREE_VALUE (c) = build_tree_list (TREE_PURPOSE (TREE_VALUE (c)),
					copy_node (CONTRACT_STATEMENT (c)));
      /* This is the copied contract statement.  */
      tree stmt = CONTRACT_STATEMENT (c);

      /* If we have an erroneous postcondition identifier, we also mark the
	 condition as invalid so only need to check that.  */
      if (CONTRACT_CONDITION (stmt) != error_mark_node)
	remap_contract (source, dest, stmt, /*duplicate_p=*/true);

      if (TREE_CODE (stmt) == POSTCONDITION_STMT)
	{
	  /* If we have a postcondition return value placeholder, then
	     ensure the copied one has the correct context.  */
	  tree var = POSTCONDITION_IDENTIFIER (stmt);
	  if (var && var != error_mark_node)
	    DECL_CONTEXT (var) = dest;
	}

      if (CONTRACT_COMMENT (stmt) != error_mark_node)
	CONTRACT_COMMENT (stmt) = copy_node (CONTRACT_COMMENT (stmt));

      chainon (last, c);
      last = c;
      if (!contracts_copy)
	contracts_copy = c;
    }

  return contracts_copy;
}

/* Set the (maybe) parsed contract specifier LIST for DECL.  */

void
set_fn_contract_specifiers (tree decl, tree list)
{
  if (!decl || error_operand_p (decl))
    return;

  bool existed = false;
  contract_decl& rd
    = hash_map_safe_get_or_insert<hm_ggc> (contract_decl_map, decl, &existed);
  if (!existed)
    {
      /* This is the first time we encountered this decl, save the location
	 for error messages.  This will ensure all error messages refer to the
	 contracts used for the function.  */
      location_t decl_loc = DECL_SOURCE_LOCATION (decl);
      location_t cont_end = decl_loc;
      if (list)
	cont_end = get_contract_end_loc (list);
      rd.note_loc = make_location (decl_loc, decl_loc, cont_end);
    }
  rd.contract_specifiers = list;
}

/* Update the entry for DECL in the map of contract specifiers with the
  contracts in LIST. */

void
update_fn_contract_specifiers (tree decl, tree list)
{
  if (!decl || error_operand_p (decl))
    return;

  bool existed = false;
  contract_decl& rd
    = hash_map_safe_get_or_insert<hm_ggc> (contract_decl_map, decl, &existed);
  gcc_checking_assert (existed);

  /* We should only get here when we parse deferred contracts.  */
  gcc_checking_assert (!contract_any_deferred_p (list));

  rd.contract_specifiers = list;
}

/* When a decl is about to be removed, then we need to release its content and
   then take it out of the map.  */

void
remove_decl_with_fn_contracts_specifiers (tree decl)
{
  if (contract_decl *p = hash_map_safe_get (contract_decl_map, decl))
    {
      p->contract_specifiers = NULL_TREE;
      contract_decl_map->remove (decl);
    }
}

/* If this function has contract specifiers, then remove them, but leave the
   function registered.  */

void
remove_fn_contract_specifiers (tree decl)
{
  if (contract_decl *p = hash_map_safe_get (contract_decl_map, decl))
    {
      p->contract_specifiers = NULL_TREE;
    }
}

/* Get the contract specifier list for this DECL if there is one.  */

tree
get_fn_contract_specifiers (tree decl)
{
  if (contract_decl *p = hash_map_safe_get (contract_decl_map, decl))
    return p->contract_specifiers;
  return NULL_TREE;
}

/* A subroutine of duplicate_decls. Diagnose issues in the redeclaration of
   guarded functions.  */

void
check_redecl_contract (tree newdecl, tree olddecl)
{
  if (!flag_contracts)
    return;

  if (TREE_CODE (newdecl) == TEMPLATE_DECL)
    newdecl = DECL_TEMPLATE_RESULT (newdecl);
  if (TREE_CODE (olddecl) == TEMPLATE_DECL)
    olddecl = DECL_TEMPLATE_RESULT (olddecl);

  tree new_contracts = get_fn_contract_specifiers (newdecl);
  tree old_contracts = get_fn_contract_specifiers (olddecl);

  if (!old_contracts && !new_contracts)
    return;

  /* We should always be comparing with the 'first' declaration which should
   have been recorded already (if it has contract specifiers).  However
   if the new decl is trying to add contracts, that is an error and we do
   not want to create a map entry yet.  */
  contract_decl *rdp = hash_map_safe_get (contract_decl_map, olddecl);
  gcc_checking_assert(rdp || !old_contracts);

  location_t new_loc = DECL_SOURCE_LOCATION (newdecl);
  if (new_contracts && !old_contracts)
    {
      auto_diagnostic_group d;
      /* If a re-declaration has contracts, they must be the same as those
       that appear on the first declaration seen (they cannot be added).  */
      location_t cont_end = get_contract_end_loc (new_contracts);
      cont_end = make_location (new_loc, new_loc, cont_end);
      error_at (cont_end, "declaration adds contracts to %q#D", olddecl);
      inform (DECL_SOURCE_LOCATION (olddecl), "first declared here");
      return;
    }

  if (old_contracts && !new_contracts)
    /* We allow re-declarations to omit contracts declared on the initial decl.
       In fact, this is required if the conditions contain lambdas.  Check if
       all the parameters are correctly const qualified. */
    check_postconditions_in_redecl (olddecl, newdecl);
  else if (old_contracts && new_contracts
	   && !contract_any_deferred_p (old_contracts)
	   && contract_any_deferred_p (new_contracts)
	   && DECL_UNIQUE_FRIEND_P (newdecl))
    {
      /* Put the defered contracts on the olddecl so we parse it when
	 we can.  */
      set_fn_contract_specifiers (olddecl, old_contracts);
    }
  else if (contract_any_deferred_p (old_contracts)
	   || contract_any_deferred_p (new_contracts))
    {
      /* TODO: ignore these and figure out how to process them later.  */
      /* Note that a friend declaration has deferred contracts, but the
	 declaration of the same function outside the class definition
	 doesn't.  */
    }
  else
    {
      gcc_checking_assert (old_contracts);
      location_t cont_end = get_contract_end_loc (new_contracts);
      cont_end = make_location (new_loc, new_loc, cont_end);
      /* We have two sets - they should match or we issue a diagnostic.  */
      match_contract_specifiers (rdp->note_loc, old_contracts,
				 cont_end, new_contracts);
    }

  return;
}

/* Update the contracts of DEST to match the argument names from contracts
  of SRC. When we merge two declarations in duplicate_decls, we preserve the
  arguments from the new declaration, if the new declaration is a
  definition. We need to update the contracts accordingly.  */

void
update_contract_arguments (tree srcdecl, tree destdecl)
{
  tree src_contracts = get_fn_contract_specifiers (srcdecl);
  tree dest_contracts = get_fn_contract_specifiers (destdecl);

  if (!src_contracts && !dest_contracts)
    return;

  /* Check if src even has contracts. It is possible that a redeclaration
    does not have contracts. Is this is the case, first apply contracts
    to src.  */
  if (!src_contracts)
    {
      if (contract_any_deferred_p (dest_contracts))
	{
	  set_fn_contract_specifiers (srcdecl, dest_contracts);
	  /* Nothing more to do here.  */
	  return;
	}
      else
	set_fn_contract_specifiers
	  (srcdecl, copy_and_remap_contracts (srcdecl, destdecl));
    }

  /* For deferred contracts, we currently copy the tokens from the redeclaration
    onto the decl that will be preserved. This is not ideal because the
    redeclaration may have erroneous contracts.
    For non deferred contracts we currently do copy and remap, which is doing
    more than we need.  */
  if (contract_any_deferred_p (src_contracts))
    set_fn_contract_specifiers (destdecl, src_contracts);
  else
    {
      /* Temporarily rename the arguments to get the right mapping.  */
      tree tmp_arguments = DECL_ARGUMENTS (destdecl);
      DECL_ARGUMENTS (destdecl) = DECL_ARGUMENTS (srcdecl);
      set_fn_contract_specifiers (destdecl,
				  copy_and_remap_contracts (destdecl, srcdecl));
      DECL_ARGUMENTS (destdecl) = tmp_arguments;
    }
}

/* Checks if a contract check wrapper is needed for fndecl.  */

static bool
should_contract_wrap_call (bool do_pre, bool do_post)
{
  /* Only if the target function actually has any contracts.  */
  if (!do_pre && !do_post)
    return false;


  return ((flag_contract_client_check > 1)
	  || ((flag_contract_client_check > 0)
	      && do_pre));
}

/* Possibly replace call with a call to a wrapper function which
   will do the contracts check required around a CALL to FNDECL.  */

tree
maybe_contract_wrap_call (tree fndecl, tree call)
{
  /* We can be called from build_cxx_call without a known callee.  */
  if (!fndecl)
    return call;

  if (error_operand_p (fndecl) || !call || call == error_mark_node)
    return error_mark_node;

  if (!handle_contracts_p (fndecl))
    return call;

  bool do_pre = has_active_preconditions (fndecl);
  bool do_post = has_active_postconditions (fndecl);

  /* Check if we need a wrapper.  */
  if (!should_contract_wrap_call (do_pre, do_post))
    return call;

  /* Build the declaration of the wrapper, if we need to.  */
  tree wrapdecl = get_or_create_contract_wrapper_function (fndecl);

  unsigned nargs = call_expr_nargs (call);
  vec<tree, va_gc> *argwrap;
  vec_alloc (argwrap, nargs);

  tree arg;
  call_expr_arg_iterator iter;
  FOR_EACH_CALL_EXPR_ARG (arg, iter, call)
    argwrap->quick_push (arg);

  tree wrapcall = build_call_expr_loc_vec (DECL_SOURCE_LOCATION (wrapdecl),
					   wrapdecl, argwrap);

  return wrapcall;
}

/* Map traversal callback to define a wrapper function.
   This generates code for client-side contract check wrappers and the
   noexcept wrapper around the contract violation handler.  */

bool
define_contract_wrapper_func (const tree& fndecl, const tree& wrapdecl, void*)
{
  /* If we already built this function on a previous pass, then do nothing.  */
  if (DECL_INITIAL (wrapdecl) && DECL_INITIAL (wrapdecl) != error_mark_node)
    return true;

  gcc_checking_assert (!DECL_HAS_CONTRACTS_P (wrapdecl));
  /* We check postconditions if postcondition checks are enabled for clients.
    We should not get here unless there are some checks to make.  */
  bool check_post = flag_contract_client_check > 1;
  /* For wrappers on CDTORs we need to refer to the original contracts,
     when the wrapper is around a clone.  */
  set_fn_contract_specifiers ( wrapdecl,
		      copy_and_remap_contracts (wrapdecl, DECL_ORIGIN (fndecl),
						check_post? cmk_all : cmk_pre));

  start_preparsed_function (wrapdecl, /*DECL_ATTRIBUTES*/NULL_TREE,
			    SF_DEFAULT | SF_PRE_PARSED);
  tree body = begin_function_body ();
  tree compound_stmt = begin_compound_stmt (BCS_FN_BODY);

  vec<tree, va_gc> * args = build_arg_list (wrapdecl);

  /* We do not support contracts on virtual functions yet.  */
  gcc_checking_assert (!DECL_IOBJ_MEMBER_FUNCTION_P (fndecl)
		       || !DECL_VIRTUAL_P (fndecl));

  tree call = build_thunk_like_call (fndecl, args->length (), args->address ());

  finish_return_stmt (call);

  finish_compound_stmt (compound_stmt);
  finish_function_body (body);
  expand_or_defer_fn (finish_function (/*inline_p=*/false));
  return true;
}

/* If any wrapper functions have been declared, emit their definition.
   This might be called multiple times, as we instantiate functions. When
   the processing here adds more wrappers, then flag to the caller that
   possible additional instantiations should be considered.
   Once instantiations are complete, this will be called with done == true.  */

bool
emit_contract_wrapper_func (bool done)
{
  if (!decl_wrapper_fn || decl_wrapper_fn->is_empty ())
    return false;
  size_t start_elements = decl_wrapper_fn->elements ();
  decl_wrapper_fn->traverse<void *, define_contract_wrapper_func>(NULL);
  bool more = decl_wrapper_fn->elements () > start_elements;
  if (done)
    decl_wrapper_fn->empty ();
  gcc_checking_assert (!done || !more);
  return more;
}

/* Mark most of a contract as being invalid.  */

tree
invalidate_contract (tree contract)
{
  if (TREE_CODE (contract) == POSTCONDITION_STMT
      && POSTCONDITION_IDENTIFIER (contract))
    POSTCONDITION_IDENTIFIER (contract) = error_mark_node;
  CONTRACT_CONDITION (contract) = error_mark_node;
  CONTRACT_COMMENT (contract) = error_mark_node;
  return contract;
}

/* Returns an invented parameter declaration of the form 'TYPE ID' for the
   purpose of parsing the postcondition.

   We use a PARM_DECL instead of a VAR_DECL so that tsubst forces a lookup
   in local specializations when we instantiate these things later.  */

tree
make_postcondition_variable (cp_expr id, tree type)
{
  if (id == error_mark_node)
    return id;
  gcc_checking_assert (scope_chain && scope_chain->bindings
		       && scope_chain->bindings->kind == sk_contract);

  tree decl = build_lang_decl (PARM_DECL, id, type);
  DECL_ARTIFICIAL (decl) = true;
  DECL_SOURCE_LOCATION (decl) = id.get_location ();
  return pushdecl (decl);
}

/* As above, except that the type is unknown.  */

tree
make_postcondition_variable (cp_expr id)
{
  return make_postcondition_variable (id, make_auto ());
}

/* Check that the TYPE is valid for a named postcondition variable on
   function decl FNDECL. Emit a diagnostic if it is not.  Returns TRUE if
   the result is OK and false otherwise.  */

bool
check_postcondition_result (tree fndecl, tree type, location_t loc)
{
  /* Do not be confused by targetm.cxx.cdtor_return_this ();
     conceptually, cdtors have no return value.  */
  if (VOID_TYPE_P (type)
      || DECL_CONSTRUCTOR_P (fndecl)
      || DECL_DESTRUCTOR_P (fndecl))
    {
      error_at (loc,
		DECL_CONSTRUCTOR_P (fndecl)
		? G_("constructor does not return a value to test")
		: DECL_DESTRUCTOR_P (fndecl)
		? G_("destructor does not return a value to test")
		: G_("function does not return a value to test"));
      return false;
    }

  return true;
}

/* Instantiate each postcondition with the return type to finalize the
   contract specifiers on a function decl FNDECL.  */

void
rebuild_postconditions (tree fndecl)
{
  if (!fndecl || fndecl == error_mark_node)
    return;

  tree type = TREE_TYPE (TREE_TYPE (fndecl));

  /* If the return type is undeduced, defer until later.  */
  if (TREE_CODE (type) == TEMPLATE_TYPE_PARM)
    return;

  tree contract_spec = get_fn_contract_specifiers (fndecl);
  for (; contract_spec ; contract_spec = TREE_CHAIN (contract_spec))
    {
      tree contract = TREE_VALUE (TREE_VALUE (contract_spec));
      if (TREE_CODE (contract) != POSTCONDITION_STMT)
	continue;
      tree condition = CONTRACT_CONDITION (contract);
      if (!condition || condition == error_mark_node)
	continue;

      /* If any conditions are deferred, they're all deferred.  Note that
	 we don't have to instantiate postconditions in that case because
	 the type is available through the declaration.  */
      if (TREE_CODE (condition) == DEFERRED_PARSE)
	return;

      tree oldvar = POSTCONDITION_IDENTIFIER (contract);
      if (!oldvar)
	continue;

      gcc_checking_assert (!DECL_CONTEXT (oldvar)
			   || DECL_CONTEXT (oldvar) == fndecl);
      DECL_CONTEXT (oldvar) = fndecl;

      /* Check the postcondition variable.  */
      location_t loc = DECL_SOURCE_LOCATION (oldvar);
      if (!check_postcondition_result (fndecl, type, loc))
	{
	  invalidate_contract (contract);
	  continue;
	}

      /* "Instantiate" the result variable using the known type.  */
      tree newvar = copy_node (oldvar);
      TREE_TYPE (newvar) = type;

      /* Make parameters and result available for substitution.  */
      local_specialization_stack stack (lss_copy);
      for (tree t = DECL_ARGUMENTS (fndecl); t != NULL_TREE; t = TREE_CHAIN (t))
	register_local_identity (t);
      register_local_specialization (newvar, oldvar);

      begin_scope (sk_contract, fndecl);
      bool old_pc = processing_postcondition;
      processing_postcondition = true;

      condition = tsubst_expr (condition, make_tree_vec (0),
			       tf_warning_or_error, fndecl);

      /* Update the contract condition and result.  */
      POSTCONDITION_IDENTIFIER (contract) = newvar;
      CONTRACT_CONDITION (contract) = finish_contract_condition (condition);
      processing_postcondition = old_pc;
      gcc_checking_assert (scope_chain && scope_chain->bindings
			   && scope_chain->bindings->kind == sk_contract);
      pop_bindings_and_leave_scope ();
    }
}

/* Make a string of the contract condition, if it is available.  */

static tree
build_comment (cp_expr condition)
{
  /* Try to get the actual source text for the condition; if that fails pretty
     print the resulting tree.  */
  char *str = get_source_text_between (global_dc->get_file_cache (),
				       condition.get_start (),
				       condition.get_finish ());
  if (!str)
    {
      const char *str = expr_to_string (condition);
      return build_string_literal (strlen (str) + 1, str);
    }

  tree t = build_string_literal (strlen (str) + 1, str);
  free (str);
  return t;
}

/* Build a contract statement.  */

tree
grok_contract (tree contract_spec, tree mode, tree result, cp_expr condition,
	       location_t loc)
{
  if (condition == error_mark_node)
    return error_mark_node;

  tree_code code;
  contract_assertion_kind kind = CAK_INVALID;
  if (id_equal (contract_spec, "contract_assert"))
    {
      code = ASSERTION_STMT;
      kind = CAK_ASSERT;
    }
  else if (id_equal (contract_spec, "pre"))
    {
      code = PRECONDITION_STMT;
      kind = CAK_PRE;
    }
  else if (id_equal (contract_spec,"post"))
    {
      code = POSTCONDITION_STMT;
      kind = CAK_POST;
    }
  else
    gcc_unreachable ();

  /* Build the contract. The condition is added later.  In the case that
     the contract is deferred, result an plain identifier, not a result
     variable.  */
  tree contract;
  if (code != POSTCONDITION_STMT)
    contract = build5_loc (loc, code, void_type_node, mode,
			   NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE);
  else
    {
      contract = build_nt (code, mode, NULL_TREE, NULL_TREE,
			   NULL_TREE, NULL_TREE, result);
      TREE_TYPE (contract) = void_type_node;
      SET_EXPR_LOCATION (contract, loc);
    }

  /* Determine the assertion kind.  */
  CONTRACT_ASSERTION_KIND (contract) = build_int_cst (uint16_type_node, kind);

  /* Determine the evaluation semantic.  This is now an override, so that if
     not set we will get the default (currently enforce).  */
  CONTRACT_EVALUATION_SEMANTIC (contract)
    = build_int_cst (uint16_type_node, (uint16_t)
		     flag_contract_evaluation_semantic);

  /* If the contract is deferred, don't do anything with the condition.  */
  if (TREE_CODE (condition) == DEFERRED_PARSE)
    {
      CONTRACT_CONDITION (contract) = condition;
      return contract;
    }

  /* Generate the comment from the original condition.  */
  CONTRACT_COMMENT (contract) = build_comment (condition);

  /* The condition is converted to bool.  */
  condition = finish_contract_condition (condition);

  if (condition == error_mark_node)
    return error_mark_node;

  CONTRACT_CONDITION (contract) = condition;

  return contract;
}

/* Build the contract specifier where IDENTIFIER is one of 'pre',
   'post' or 'assert' and CONTRACT is the underlying statement.  */

tree
finish_contract_specifier (tree identifier, tree contract)
{
  if (contract == error_mark_node)
    return error_mark_node;

  tree contract_spec = build_tree_list (build_tree_list (NULL_TREE, identifier),
					build_tree_list (NULL_TREE, contract));

  /* Mark the contract as dependent if the condition is dependent.  */
  tree condition = CONTRACT_CONDITION (contract);
  if (TREE_CODE (condition) != DEFERRED_PARSE
      && value_dependent_expression_p (condition))
    ATTR_IS_DEPENDENT (contract_spec) = true;

  return contract_spec;
}

/* Update condition of a late-parsed contract and postcondition variable,
   if any.  */

void
update_late_contract (tree contract, tree result, cp_expr condition)
{
  if (TREE_CODE (contract) == POSTCONDITION_STMT)
    POSTCONDITION_IDENTIFIER (contract) = result;

  /* Generate the comment from the original condition.  */
  CONTRACT_COMMENT (contract) = build_comment (condition);

  /* The condition is converted to bool.  */
  condition = finish_contract_condition (condition);
  CONTRACT_CONDITION (contract) = condition;
}

/* Returns the precondition funtion for FNDECL, or null if not set.  */

tree
get_precondition_function (tree fndecl)
{
  gcc_checking_assert (fndecl);
  tree *result = hash_map_safe_get (decl_pre_fn, fndecl);
  return result ? *result : NULL_TREE;
}

/* Returns the postcondition funtion for FNDECL, or null if not set.  */

tree
get_postcondition_function (tree fndecl)
{
  gcc_checking_assert (fndecl);
  tree *result = hash_map_safe_get (decl_post_fn, fndecl);
  return result ? *result : NULL_TREE;
}

/* Set the PRE and POST functions for FNDECL.  Note that PRE and POST can
   be null in this case.  If so the functions are not recorded.  Used by the
   modules code.  */

void
set_contract_functions (tree fndecl, tree pre, tree post)
{
  if (pre)
    set_precondition_function (fndecl, pre);

  if (post)
    set_postcondition_function (fndecl, post);
}


/* We're compiling the pre/postcondition function CONDFN; remap any FN
   contracts that match CODE and emit them.  */

static void
remap_and_emit_conditions (tree fn, tree condfn, tree_code code)
{
  gcc_assert (code == PRECONDITION_STMT || code == POSTCONDITION_STMT);
  tree contract_spec = get_fn_contract_specifiers (fn);
  for (; contract_spec; contract_spec = TREE_CHAIN (contract_spec))
    {
      tree contract = CONTRACT_STATEMENT (contract_spec);
      if (TREE_CODE (contract) == code)
	{
	  contract = copy_node (contract);
	  if (CONTRACT_CONDITION (contract) != error_mark_node)
	    remap_contract (fn, condfn, contract, /*duplicate_p=*/false);
	  emit_contract_statement (contract);
	}
    }
}

/* Finish up the pre & post function definitions for a guarded FNDECL,
   and compile those functions all the way to assembler language output.  */

void
finish_function_outlined_contracts (tree fndecl)
{
  /* If the guarded func is either already decided to be ill-formed or is
     not yet complete return early.  */
  if (error_operand_p (fndecl)
      || !DECL_INITIAL (fndecl)
      || DECL_INITIAL (fndecl) == error_mark_node)
    return;

  /* If there are no contracts here, or we're building them in-line then we
     do not need to build the outlined functions.  */
  if (!handle_contracts_p (fndecl)
      || !flag_contract_checks_outlined)
    return;

  /* If this is not a client side check and definition side checks are
     disabled, do nothing.  */
  if (!flag_contracts_definition_check
      && !DECL_CONTRACT_WRAPPER (fndecl))
    return;

  /* If either the pre or post functions are bad, don't bother emitting
     any contracts.  The program is already ill-formed.  */
  tree pre = DECL_PRE_FN (fndecl);
  tree post = DECL_POST_FN (fndecl);
  if (pre == error_mark_node || post == error_mark_node)
    return;

  /* We are generating code, deferred parses should be complete.  */
  tree contract_spec = get_fn_contract_specifiers (fndecl);
  gcc_checking_assert (!contract_any_deferred_p (contract_spec));

  int flags = SF_DEFAULT | SF_PRE_PARSED;

  if (pre && !DECL_INITIAL (pre))
    {
      DECL_PENDING_INLINE_P (pre) = false;
      start_preparsed_function (pre, DECL_ATTRIBUTES (pre), flags);
      remap_and_emit_conditions (fndecl, pre, PRECONDITION_STMT);
      finish_return_stmt (NULL_TREE);
      pre = finish_function (false);
      expand_or_defer_fn (pre);
    }

  if (post && !DECL_INITIAL (post))
    {
      DECL_PENDING_INLINE_P (post) = false;
      start_preparsed_function (post, DECL_ATTRIBUTES (post), flags);
      remap_and_emit_conditions (fndecl, post, POSTCONDITION_STMT);
      gcc_checking_assert (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (post))));
      finish_return_stmt (NULL_TREE);
      post = finish_function (false);
      expand_or_defer_fn (post);
    }
}

/* ===== Code generation ===== */

/* Insert a BUILT_IN_OBSERVABLE_CHECKPOINT epoch marker.  */

static void
emit_builtin_observable_checkpoint ()
{
  tree fn = builtin_decl_explicit (BUILT_IN_OBSERVABLE_CHKPT);
  releasing_vec vec;
  fn = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  finish_expr_stmt (fn);
}

/* Shared code between TU-local wrappers for the violation handler.  */

static tree
declare_one_violation_handler_wrapper (tree fn_name, tree fn_type,
				       tree p1_type, tree p2_type)
{
  location_t loc = BUILTINS_LOCATION;
  tree fn_decl = build_lang_decl_loc (loc, FUNCTION_DECL, fn_name, fn_type);
  DECL_CONTEXT (fn_decl) = FROB_CONTEXT (global_namespace);
  DECL_ARTIFICIAL (fn_decl) = true;
  DECL_INITIAL (fn_decl) = error_mark_node;
  /* Let the start function code fill in the result decl.  */
  DECL_RESULT (fn_decl) = NULL_TREE;
  /* Two args violation ref, dynamic info.  */
  tree parms = cp_build_parm_decl (fn_decl, NULL_TREE, p1_type);
  TREE_USED (parms) = true;
  DECL_READ_P (parms) = true;
  tree p2 = cp_build_parm_decl (fn_decl, NULL_TREE, p2_type);
  TREE_USED (p2) = true;
  DECL_READ_P (p2) = true;
  DECL_CHAIN (parms) = p2;
  DECL_ARGUMENTS (fn_decl) = parms;
  /* Make this function internal.  */
  TREE_PUBLIC (fn_decl) = false;
  DECL_EXTERNAL (fn_decl) = false;
  DECL_WEAK (fn_decl) = false;
  return fn_decl;
}

static GTY(()) tree tu_has_violation = NULL_TREE;
static GTY(()) tree tu_has_violation_exception = NULL_TREE;

static void
declare_violation_handler_wrappers ()
{
  if (tu_has_violation && tu_has_violation_exception)
    return;

  iloc_sentinel ils (input_location);
  input_location = BUILTINS_LOCATION;
  tree v_obj_type = builtin_contract_violation_type;
  v_obj_type = cp_build_qualified_type (v_obj_type, TYPE_QUAL_CONST);
  v_obj_type = cp_build_reference_type (v_obj_type, /*rval*/false);
  tree fn_type = build_function_type_list (void_type_node, v_obj_type,
					   uint16_type_node, NULL_TREE);
  tree fn_name = get_identifier ("__tu_has_violation_exception");
  tu_has_violation_exception
    = declare_one_violation_handler_wrapper (fn_name, fn_type, v_obj_type,
					     uint16_type_node);
  fn_name = get_identifier ("__tu_has_violation");
  tu_has_violation
    = declare_one_violation_handler_wrapper (fn_name, fn_type, v_obj_type,
					     uint16_type_node);
}

static GTY(()) tree tu_terminate_wrapper = NULL_TREE;

/* Declare a noipa wrapper around the call to std::terminate */

static tree
declare_terminate_wrapper ()
{
  if (tu_terminate_wrapper)
    return tu_terminate_wrapper;

  iloc_sentinel ils (input_location);
  input_location = BUILTINS_LOCATION;

  tree fn_type = build_function_type_list (void_type_node, NULL_TREE);
  if (!TREE_NOTHROW (terminate_fn))
    fn_type = build_exception_variant (fn_type, noexcept_true_spec);
  tree fn_name = get_identifier ("__tu_terminate_wrapper");

  tu_terminate_wrapper
    = build_lang_decl_loc (input_location, FUNCTION_DECL, fn_name, fn_type);
  DECL_CONTEXT (tu_terminate_wrapper) = FROB_CONTEXT(global_namespace);
  DECL_ARTIFICIAL (tu_terminate_wrapper) = true;
  DECL_INITIAL (tu_terminate_wrapper) = error_mark_node;
  /* Let the start function code fill in the result decl.  */
  DECL_RESULT (tu_terminate_wrapper) = NULL_TREE;

  /* Make this function internal.  */
  TREE_PUBLIC (tu_terminate_wrapper) = false;
  DECL_EXTERNAL (tu_terminate_wrapper) = false;
  DECL_WEAK (tu_terminate_wrapper) = false;

  DECL_ATTRIBUTES (tu_terminate_wrapper)
    = tree_cons (get_identifier ("noipa"), NULL, NULL_TREE);
  cplus_decl_attributes (&tu_terminate_wrapper,
			 DECL_ATTRIBUTES (tu_terminate_wrapper), 0);
  return tu_terminate_wrapper;
}

/* Define a noipa wrapper around the call to std::terminate */

static void
build_terminate_wrapper ()
{
  /* We should not be trying to build this if we never used it.  */
  gcc_checking_assert (tu_terminate_wrapper);

  start_preparsed_function (tu_terminate_wrapper,
			    DECL_ATTRIBUTES(tu_terminate_wrapper),
			    SF_DEFAULT | SF_PRE_PARSED);
  tree body = begin_function_body ();
  tree compound_stmt = begin_compound_stmt (BCS_FN_BODY);
  finish_expr_stmt (build_call_a (terminate_fn, 0, nullptr));
  finish_return_stmt (NULL_TREE);
  finish_compound_stmt (compound_stmt);
  finish_function_body (body);
  tu_terminate_wrapper = finish_function (false);
  expand_or_defer_fn (tu_terminate_wrapper);
}

/* Lookup a name in std::contracts, or inject it.  */

static tree
lookup_std_contracts_type (tree name_id)
{
  tree id_ns = get_identifier ("contracts");
  tree ns = lookup_qualified_name (std_node, id_ns);

  tree res_type = error_mark_node;
  if (TREE_CODE (ns) == NAMESPACE_DECL)
    res_type = lookup_qualified_name
      (ns, name_id, LOOK_want::TYPE | LOOK_want::HIDDEN_FRIEND);

  if (TREE_CODE (res_type) == TYPE_DECL)
    res_type = TREE_TYPE (res_type);
  else
    {
      push_nested_namespace (std_node);
      push_namespace (id_ns, /*inline*/false);
      res_type = make_class_type (RECORD_TYPE);
      create_implicit_typedef (name_id, res_type);
      DECL_SOURCE_LOCATION (TYPE_NAME (res_type)) = BUILTINS_LOCATION;
      DECL_CONTEXT (TYPE_NAME (res_type)) = current_namespace;
      pushdecl_namespace_level (TYPE_NAME (res_type), /*hidden*/true);
      pop_namespace ();
      pop_nested_namespace (std_node);
    }
  return res_type;
}

/* Return handle_contract_violation (), declaring it if needed.  */

static tree
declare_handle_contract_violation ()
{
  /* We may need to declare new types, ensure they are not considered
     attached to a named module.  */
  auto module_kind_override = make_temp_override
    (module_kind, module_kind & ~(MK_PURVIEW | MK_ATTACH | MK_EXPORTING));
  tree fnname = get_identifier ("handle_contract_violation");
  tree viol_name = get_identifier ("contract_violation");
  tree l = lookup_qualified_name (global_namespace, fnname,
				  LOOK_want::HIDDEN_FRIEND);
  for (tree f: lkp_range (l))
    if (TREE_CODE (f) == FUNCTION_DECL)
	{
	  tree parms = TYPE_ARG_TYPES (TREE_TYPE (f));
	  if (remaining_arguments (parms) != 1)
	    continue;
	  tree parmtype = non_reference (TREE_VALUE (parms));
	  if (CLASS_TYPE_P (parmtype)
	      && TYPE_IDENTIFIER (parmtype) == viol_name)
	    return f;
	}

  tree violation = lookup_std_contracts_type (viol_name);
  tree fntype = NULL_TREE;
  tree v_obj_ref = cp_build_qualified_type (violation, TYPE_QUAL_CONST);
  v_obj_ref = cp_build_reference_type (v_obj_ref, /*rval*/false);
  fntype = build_function_type_list (void_type_node, v_obj_ref, NULL_TREE);

  push_nested_namespace (global_namespace);
  tree fndecl
    = build_cp_library_fn_ptr ("handle_contract_violation", fntype, ECF_COLD);
  pushdecl_namespace_level (fndecl, /*hiding*/true);
  pop_nested_namespace (global_namespace);

  /* Build the parameter(s).  */
  tree parms = cp_build_parm_decl (fndecl, NULL_TREE, v_obj_ref);
  TREE_USED (parms) = true;
  DECL_READ_P (parms) = true;
  DECL_ARGUMENTS (fndecl) = parms;
  return fndecl;
}

/* Build the call to handle_contract_violation for VIOLATION.  */

static void
build_contract_handler_call (tree violation)
{
  tree violation_fn = declare_handle_contract_violation ();
  tree call = build_call_n (violation_fn, 1, violation);
  finish_expr_stmt (call);
}

/* If we have emitted any contracts in this TU that will call a violation
   handler, then emit the wrappers for the handler.  */

void
maybe_emit_violation_handler_wrappers ()
{
  /* We might need the terminate wrapper, even if we do not use the violation
     handler wrappers.  */
  if (tu_terminate_wrapper && flag_contracts_conservative_ipa)
    build_terminate_wrapper ();

  if (!tu_has_violation && !tu_has_violation_exception)
    return;

  tree terminate_wrapper = terminate_fn;
  if (flag_contracts_conservative_ipa)
    terminate_wrapper = tu_terminate_wrapper;

  /* tu_has_violation */
  start_preparsed_function (tu_has_violation, NULL_TREE,
			    SF_DEFAULT | SF_PRE_PARSED);
  tree body = begin_function_body ();
  tree compound_stmt = begin_compound_stmt (BCS_FN_BODY);
  tree v = DECL_ARGUMENTS (tu_has_violation);
  tree semantic = DECL_CHAIN (v);

  /* We are going to call the handler.  */
  build_contract_handler_call (v);

  tree if_observe = begin_if_stmt ();
  /* if (observe) return; */
  tree cond = build2 (EQ_EXPR, uint16_type_node, semantic,
		      build_int_cst (uint16_type_node, (uint16_t)CES_OBSERVE));
  finish_if_stmt_cond (cond, if_observe);
  emit_builtin_observable_checkpoint ();
  finish_then_clause (if_observe);
  begin_else_clause (if_observe);
  /* else terminate.  */
  finish_expr_stmt (build_call_a (terminate_wrapper, 0, nullptr));
  finish_else_clause (if_observe);
  finish_if_stmt (if_observe);
  finish_return_stmt (NULL_TREE);

  finish_compound_stmt (compound_stmt);
  finish_function_body (body);
  tu_has_violation = finish_function (false);
  expand_or_defer_fn (tu_has_violation);

  /* tu_has_violation_exception */
  start_preparsed_function (tu_has_violation_exception, NULL_TREE,
			    SF_DEFAULT | SF_PRE_PARSED);
  body = begin_function_body ();
  compound_stmt = begin_compound_stmt (BCS_FN_BODY);
  v = DECL_ARGUMENTS (tu_has_violation_exception);
  semantic = DECL_CHAIN (v);
  location_t loc = DECL_SOURCE_LOCATION (tu_has_violation_exception);

  tree a_type = strip_top_quals (non_reference (TREE_TYPE (v)));
  tree v2 = build_decl (loc, VAR_DECL, NULL_TREE, a_type);
  DECL_SOURCE_LOCATION (v2) = loc;
  DECL_CONTEXT (v2) = current_function_decl;
  DECL_ARTIFICIAL (v2) = true;
  layout_decl (v2, 0);
  v2 = pushdecl (v2);
  add_decl_expr (v2);
  tree r = cp_build_init_expr (v2, convert_from_reference (v));
  finish_expr_stmt (r);
  tree memb = lookup_member (a_type, get_identifier ("_M_detection_mode"),
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
  r = build_class_member_access_expr (v2, memb, NULL_TREE, false,
				      tf_warning_or_error);
  r = cp_build_modify_expr
   (loc, r, NOP_EXPR,
    build_int_cst (uint16_type_node, (uint16_t)CDM_EVAL_EXCEPTION),
    tf_warning_or_error);
  finish_expr_stmt (r);
  /* We are going to call the handler.  */
  build_contract_handler_call (v);

  if_observe = begin_if_stmt ();
  /* if (observe) return; */
  cond = build2 (EQ_EXPR, uint16_type_node, semantic,
		 build_int_cst (uint16_type_node, (uint16_t)CES_OBSERVE));
  finish_if_stmt_cond (cond, if_observe);
  emit_builtin_observable_checkpoint ();
  finish_then_clause (if_observe);
  begin_else_clause (if_observe);
  /* else terminate.  */
  finish_expr_stmt (build_call_a (terminate_wrapper, 0, nullptr));
  finish_else_clause (if_observe);
  finish_if_stmt (if_observe);
  finish_return_stmt (NULL_TREE);
  finish_compound_stmt (compound_stmt);
  finish_function_body (body);
  tu_has_violation_exception = finish_function (false);
  expand_or_defer_fn (tu_has_violation_exception);
}

/* Build a layout-compatible internal version of contract_violation type.  */

static tree
get_contract_violation_fields ()
{
  tree fields = NULL_TREE;
  /* Must match <contracts>:
  class contract_violation {
    uint16_t _M_version;
    assertion_kind _M_assertion_kind;
    evaluation_semantic _M_evaluation_semantic;
    detection_mode _M_detection_mode;
    const char* _M_comment;
    void *_M_src_loc_ptr;
    __vendor_ext* _M_ext;
  };
    If this changes, also update the initializer in
    build_contract_violation.  */
  const tree types[] = { uint16_type_node,
			 uint16_type_node,
			 uint16_type_node,
			 uint16_type_node,
			 const_string_type_node,
			 ptr_type_node,
			 ptr_type_node
			};
 const char *names[] = { "_M_version",
			 "_M_assertion_kind",
			 "_M_evaluation_semantic",
			 "_M_detection_mode",
			 "_M_comment",
			 "_M_src_loc_ptr",
			 "_M_ext",
			};
  unsigned n = 0;
  for (tree type : types)
    {
      /* finish_builtin_struct wants fields chained in reverse.  */
      tree next = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				  get_identifier(names[n++]), type);
      DECL_CHAIN (next) = fields;
      fields = next;
    }
 return fields;
}

/* Build a type to represent contract violation objects.  */

static tree
init_builtin_contract_violation_type ()
{
  if (builtin_contract_violation_type)
    return builtin_contract_violation_type;

  tree fields = get_contract_violation_fields ();

  iloc_sentinel ils (input_location);
  input_location = BUILTINS_LOCATION;
  builtin_contract_violation_type = make_class_type (RECORD_TYPE);
  finish_builtin_struct (builtin_contract_violation_type,
			 "__builtin_contract_violation_type", fields, NULL_TREE);
  CLASSTYPE_AS_BASE (builtin_contract_violation_type)
    = builtin_contract_violation_type;
  DECL_CONTEXT (TYPE_NAME (builtin_contract_violation_type))
    = FROB_CONTEXT (global_namespace);
  CLASSTYPE_LITERAL_P (builtin_contract_violation_type) = true;
  CLASSTYPE_LAZY_COPY_CTOR (builtin_contract_violation_type) = true;
  xref_basetypes (builtin_contract_violation_type, /*bases=*/NULL_TREE);
  DECL_CONTEXT (TYPE_NAME (builtin_contract_violation_type))
    = FROB_CONTEXT (global_namespace);
  DECL_ARTIFICIAL (TYPE_NAME (builtin_contract_violation_type)) = true;
  TYPE_ARTIFICIAL (builtin_contract_violation_type) = true;
  builtin_contract_violation_type
    = cp_build_qualified_type (builtin_contract_violation_type,
			       TYPE_QUAL_CONST);
  return builtin_contract_violation_type;
}

/* Early initialisation of types and functions we will use.  */
void
init_contracts ()
{
  init_terminate_fn ();
  init_builtin_contract_violation_type ();
}

static GTY(()) tree contracts_source_location_impl_type;

/* Build a layout-compatible internal version of source location __impl
   type.  */

static tree
get_contracts_source_location_impl_type (tree context = NULL_TREE)
{
  if (contracts_source_location_impl_type)
     return contracts_source_location_impl_type;

  /* First see if we have a declaration that we can use.  */
  tree contracts_source_location_type
    = lookup_std_type (get_identifier ("source_location"));

  if (contracts_source_location_type
      && contracts_source_location_type != error_mark_node
      && TYPE_FIELDS (contracts_source_location_type))
    {
      contracts_source_location_impl_type = get_source_location_impl_type ();
      return contracts_source_location_impl_type;
    }

  /* We do not, so build the __impl layout equivalent type, which must
     match <source_location>:
     struct __impl
      {
	  const char* _M_file_name;
	  const char* _M_function_name;
	  unsigned _M_line;
	  unsigned _M_column;
      }; */
  const tree types[] = { const_string_type_node,
			const_string_type_node,
			uint_least32_type_node,
			uint_least32_type_node };

 const char *names[] = { "_M_file_name",
			 "_M_function_name",
			 "_M_line",
			 "_M_column",
			};
  tree fields = NULL_TREE;
  unsigned n = 0;
  for (tree type : types)
  {
    /* finish_builtin_struct wants fields chained in reverse.  */
    tree next = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			    get_identifier (names[n++]), type);
    DECL_CHAIN (next) = fields;
    fields = next;
  }

  iloc_sentinel ils (input_location);
  input_location = BUILTINS_LOCATION;
  contracts_source_location_impl_type = cxx_make_type (RECORD_TYPE);
  finish_builtin_struct (contracts_source_location_impl_type,
			 "__impl", fields, NULL_TREE);
  DECL_CONTEXT (TYPE_NAME (contracts_source_location_impl_type)) = context;
  DECL_ARTIFICIAL (TYPE_NAME (contracts_source_location_impl_type)) = true;
  TYPE_ARTIFICIAL (contracts_source_location_impl_type) = true;
  contracts_source_location_impl_type
    = cp_build_qualified_type (contracts_source_location_impl_type,
			       TYPE_QUAL_CONST);

  return contracts_source_location_impl_type;
}

static tree
get_src_loc_impl_ptr (location_t loc)
{
  if (!contracts_source_location_impl_type)
    get_contracts_source_location_impl_type ();

  tree fndecl = current_function_decl;
  /* We might be an outlined function.  */
  if (DECL_IS_PRE_FN_P (fndecl) || DECL_IS_POST_FN_P (fndecl))
    fndecl = get_orig_for_outlined (fndecl);
  /* We might be a wrapper.  */
  if (DECL_IS_WRAPPER_FN_P (fndecl))
    fndecl = get_orig_func_for_wrapper (fndecl);

  gcc_checking_assert (fndecl);
  tree impl__
    = build_source_location_impl (loc, fndecl,
				  contracts_source_location_impl_type);
  tree p = build_pointer_type (contracts_source_location_impl_type);
  return build_fold_addr_expr_with_type_loc (loc, impl__, p);
}

/* Build a contract_violation layout compatible object. */

/* Constructor.  At present, this should always be constant. */

static tree
build_contract_violation_ctor (tree contract)
{
  bool can_be_const = true;
  uint16_t version = 1;
  /* Default CDM_PREDICATE_FALSE. */
  uint16_t detection_mode = CDM_PREDICATE_FALSE;

  tree assertion_kind = CONTRACT_ASSERTION_KIND (contract);
  if (!assertion_kind || really_constant_p (assertion_kind))
    {
      contract_assertion_kind kind = get_contract_assertion_kind (contract);
      assertion_kind = build_int_cst (uint16_type_node, kind);
    }
  else
    can_be_const = false;

  tree eval_semantic = CONTRACT_EVALUATION_SEMANTIC (contract);
  gcc_checking_assert (eval_semantic);
  if (!really_constant_p (eval_semantic))
    can_be_const = false;

  tree comment = CONTRACT_COMMENT (contract);
  if (comment && !really_constant_p (comment))
    can_be_const = false;

  tree std_src_loc_impl_ptr = CONTRACT_STD_SOURCE_LOC (contract);
  if (std_src_loc_impl_ptr)
    {
      std_src_loc_impl_ptr = convert_from_reference (std_src_loc_impl_ptr);
      if (!really_constant_p (std_src_loc_impl_ptr))
	can_be_const = false;
    }
  else
    std_src_loc_impl_ptr = get_src_loc_impl_ptr (EXPR_LOCATION (contract));

  /* Must match the type layout in builtin_contract_violation_type.  */
  tree f0 = next_aggregate_field (TYPE_FIELDS (builtin_contract_violation_type));
  tree f1 = next_aggregate_field (DECL_CHAIN (f0));
  tree f2 = next_aggregate_field (DECL_CHAIN (f1));
  tree f3 = next_aggregate_field (DECL_CHAIN (f2));
  tree f4 = next_aggregate_field (DECL_CHAIN (f3));
  tree f5 = next_aggregate_field (DECL_CHAIN (f4));
  tree f6 = next_aggregate_field (DECL_CHAIN (f5));
  tree ctor = build_constructor_va
    (builtin_contract_violation_type, 7,
     f0, build_int_cst (uint16_type_node, version),
     f1, assertion_kind,
     f2, eval_semantic,
     f3, build_int_cst (uint16_type_node, detection_mode),
     f4, comment,
     f5, std_src_loc_impl_ptr,
     f6, build_zero_cst (nullptr_type_node)); // __vendor_ext

  TREE_READONLY (ctor) = true;
  if (can_be_const)
    TREE_CONSTANT (ctor) = true;

  return ctor;
}

/* Build a named TU-local constant of TYPE.  */

static tree
contracts_tu_local_named_var (location_t loc, const char *name, tree type)
{
  tree var_ = build_decl (loc, VAR_DECL, NULL, type);
  DECL_NAME (var_) = generate_internal_label (name);
  TREE_PUBLIC (var_) = false;
  DECL_EXTERNAL (var_) = false;
  TREE_STATIC (var_) = true;
  /* Compiler-generated.  */
  DECL_ARTIFICIAL (var_) = true;
  TREE_CONSTANT (var_) = true;
  layout_decl (var_, 0);
  return var_;
}

/* Create a read-only violation object.  */

static tree
build_contract_violation_constant (tree ctor, tree contract)
{
  tree viol_ = contracts_tu_local_named_var
    (EXPR_LOCATION (contract), "Lcontract_violation",
     builtin_contract_violation_type);

  TREE_CONSTANT (viol_) = true;
  DECL_INITIAL (viol_) = ctor;
  varpool_node::finalize_decl (viol_);

  return viol_;
}

/* Helper to replace references to dummy this parameters with references to
   the first argument of the FUNCTION_DECL DATA.  */

static tree
remap_dummy_this_1 (tree *tp, int *, void *data)
{
  if (!is_this_parameter (*tp))
    return NULL_TREE;
  tree fn = (tree)data;
  *tp = DECL_ARGUMENTS (fn);
  return NULL_TREE;
}

/* Replace all references to dummy this parameters in EXPR with references to
   the first argument of the FUNCTION_DECL FNDECL.  */

static void
remap_dummy_this (tree fndecl, tree *expr)
{
  walk_tree (expr, remap_dummy_this_1, fndecl, NULL);
}

/* Replace uses of user's placeholder var with the actual return value.  */

struct replace_tree
{
  tree from, to;
};

static tree
remap_retval_1 (tree *here, int *do_subtree, void *d)
{
  replace_tree *data = (replace_tree *) d;

  if (*here == data->from)
    {
      *here = data->to;
      *do_subtree = 0;
    }
  else
    *do_subtree = 1;
  return NULL_TREE;
}

static void
remap_retval (tree fndecl, tree contract)
{
  struct replace_tree data;
  data.from = POSTCONDITION_IDENTIFIER (contract);
  gcc_checking_assert (DECL_RESULT (fndecl));
  data.to = DECL_RESULT (fndecl);
  walk_tree (&CONTRACT_CONDITION (contract), remap_retval_1, &data, NULL);
}


/* Genericize a CONTRACT tree, but do not attach it to the current context,
   the caller is responsible for that.
   This is called during genericization.  */

tree
build_contract_check (tree contract)
{
  contract_evaluation_semantic semantic = get_evaluation_semantic (contract);
  bool quick = false;
  bool calls_handler = false;
  switch (semantic)
    {
    case CES_IGNORE:
      return void_node;
    case CES_ENFORCE:
    case CES_OBSERVE:
      calls_handler = true;
      break;
    case CES_QUICK:
      quick = true;
      break;
    default:
      gcc_unreachable ();
    }

  location_t loc = EXPR_LOCATION (contract);

  remap_dummy_this (current_function_decl, &CONTRACT_CONDITION (contract));
  tree condition = CONTRACT_CONDITION (contract);
  if (condition == error_mark_node)
    return NULL_TREE;

  if (!flag_contract_checks_outlined && POSTCONDITION_P (contract))
    {
      remap_retval (current_function_decl, contract);
      condition = CONTRACT_CONDITION (contract);
      if (condition == error_mark_node)
	return NULL_TREE;
    }

  tree terminate_wrapper = terminate_fn;
  if (flag_contracts_conservative_ipa)
    terminate_wrapper = declare_terminate_wrapper ();
  if (calls_handler)
    declare_violation_handler_wrappers ();

  bool check_might_throw = (flag_exceptions
			    && !expr_noexcept_p (condition, tf_none));

  /* Build a statement expression to hold a contract check, with the check
     potentially wrapped in a try-catch expr.  */
  tree cc_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  BIND_EXPR_BODY (cc_bind) = push_stmt_list ();

  if (TREE_CODE (contract) == ASSERTION_STMT)
    emit_builtin_observable_checkpoint ();
  tree cond = build_x_unary_op (loc, TRUTH_NOT_EXPR, condition, NULL_TREE,
				tf_warning_or_error);
  tree violation;
  bool viol_is_var = false;
  if (quick)
    /* We will not be calling a handler.  */
    violation = build_zero_cst (nullptr_type_node);
  else
    {
      /* Build a violation object, with the contract settings.  */
      tree ctor = build_contract_violation_ctor (contract);
      gcc_checking_assert (TREE_CONSTANT (ctor));
      violation = build_contract_violation_constant (ctor, contract);
      violation = build_address (violation);
    }

  tree s_const = build_int_cst (uint16_type_node, semantic);
  /* So now do we need a try-catch?  */
  if (check_might_throw)
    {
      /* This will hold the computed condition.  */
      tree check_failed = build_decl (loc, VAR_DECL, NULL, boolean_type_node);
      DECL_ARTIFICIAL (check_failed) = true;
      DECL_IGNORED_P (check_failed) = true;
      DECL_CONTEXT (check_failed) = current_function_decl;
      layout_decl (check_failed, 0);
      add_decl_expr (check_failed);
      DECL_CHAIN (check_failed) = BIND_EXPR_VARS (cc_bind);
      BIND_EXPR_VARS (cc_bind) = check_failed;
      tree check_try = begin_try_block ();
      finish_expr_stmt (cp_build_init_expr (check_failed, cond));
      finish_try_block (check_try);

      tree handler = begin_handler ();
      finish_handler_parms (NULL_TREE, handler); /* catch (...) */
      if (quick)
	finish_expr_stmt (build_call_a (terminate_wrapper, 0, nullptr));
      else
	{
	  if (viol_is_var)
	    {
	      /* We can update the detection mode here.  */
	      tree memb
		= lookup_member (builtin_contract_violation_type,
				 get_identifier ("_M_detection_mode"),
				 1, 0, tf_warning_or_error);
	      tree r = cp_build_indirect_ref (loc, violation, RO_UNARY_STAR,
					      tf_warning_or_error);
	      r = build_class_member_access_expr (r, memb, NULL_TREE, false,
						  tf_warning_or_error);
	      r = cp_build_modify_expr
		(loc, r, NOP_EXPR,
		 build_int_cst (uint16_type_node, (uint16_t)CDM_EVAL_EXCEPTION),
		 tf_warning_or_error);
	      finish_expr_stmt (r);
	      finish_expr_stmt (build_call_n (tu_has_violation, 2,
					      violation, s_const));
	    }
	  else
	    /* We need to make a copy of the violation object to update.  */
	    finish_expr_stmt (build_call_n (tu_has_violation_exception, 2,
					    violation, s_const));
	  /* If we reach here, we have handled the exception thrown and do not
	     need further action.  */
	  tree e = cp_build_modify_expr (loc, check_failed, NOP_EXPR,
					 boolean_false_node,
					 tf_warning_or_error);
	  finish_expr_stmt (e);
	}
      finish_handler (handler);
      finish_handler_sequence (check_try);
      cond = check_failed;
      BIND_EXPR_VARS (cc_bind) = nreverse (BIND_EXPR_VARS (cc_bind));
    }

  tree do_check = begin_if_stmt ();
  finish_if_stmt_cond (cond, do_check);
  if (quick)
    finish_expr_stmt (build_call_a (terminate_wrapper, 0, nullptr));
  else
    finish_expr_stmt (build_call_n (tu_has_violation, 2, violation, s_const));
  finish_then_clause (do_check);
  finish_if_stmt (do_check);

  BIND_EXPR_BODY (cc_bind) = pop_stmt_list (BIND_EXPR_BODY (cc_bind));
  return cc_bind;
}

#include "gt-cp-contracts.h"
