/* Definitions for C++ contract levels
   Copyright (C) 2020-2025 Free Software Foundation, Inc.
   Contributed by Jeff Chapman II (jchapman@lock3software.com)

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

/* Design Notes

   A function is called a "guarded" function if it has pre or post contract
   attributes. A contract is considered an "active" contract if runtime code is
   needed for the contract under the current contract configuration.

   pre and post contract attributes are parsed and stored in DECL_ATTRIBUTES.
   assert contracts are parsed and wrapped in statements. When genericizing, all
   active and assumed contracts are transformed into an if block. An observed
   contract:

     [[ pre: v > 0 ]]

   is transformed into:

     if (!(v > 0)) {
       handle_contract_violation(__pseudo_contract_violation{
	 5, // line_number,
	 "main.cpp", // file_name,
	 "fun", // function_name,
	 "v > 0", // comment,
	 "default", // assertion_level,
	 "default", // assertion_role,
	 maybe_continue, // continuation_mode
       });
       terminate (); // if never_continue
     }

   We use an internal type with the same layout as contract_violation rather
   than try to define the latter internally and somehow deal with its actual
   definition in a TU that includes <contract>.

   ??? is it worth factoring out the calls to handle_contract_violation and
   terminate into a local function?

   Assumed contracts use the same implementation as C++23 [[assume]].

   Parsing of pre and post contract conditions need to be deferred when the
   contracts are attached to a member function. The postcondition identifier
   cannot be used before the deduced return type of an auto function is used,
   except when used in a defining declaration in which case they conditions are
   fully parsed once the body is finished (see cpp2a/contracts-deduced{1,2}.C).

   A list of pre and post contracts can either be repeated in their entirety or
   completely absent in subsequent declarations. If contract lists appear on two
   matching declarations, their contracts have to be equivalent. In general this
   means that anything before the colon have to be token equivalent and the
   condition must be cp_tree_equal (primarily to allow for parameter renaming).

   Contracts on overrides must match those present on (all of) the overridee(s).

   Template specializations may have their own contracts. If no contracts are
   specified on the initial specialization they're assumed to be the same as
   the primary template. Specialization redeclarations must then match either
   the primary template (if they were unspecified originally), or those
   specified on the specialization.


   For non-cdtors two functions are generated for ease of implementation and to
   avoid some cases where code bloat may occurr. These are the DECL_PRE_FN and
   DECL_POST_FN. Each handles checking either the set of pre or post contracts
   of a guarded function.

     int fun(int v)
       [[ pre: v > 0 ]]
       [[ post r: r < 0 ]]
     {
       return -v;
     }

   We implement the checking as follows:

   For functions with no post-conditions we wrap the original function body as
   follows:

   {
      handle_pre_condition_checking ();
      original_function_body ();
   }

   This implements the intent that the preconditions are processed after the
   function parameters are initialised but before any other actions.

   For functions with post-conditions:

   if (preconditions_exist)
     handle_pre_condition_checking ();
   try
     {
       original_function_body ();
     }
   finally
     {
       handle_post_condition_checking ();
     }
   else [only if the function is not marked noexcept(true) ]
     {
       ;
     }

   In this, post-conditions [that might apply to the return value etc.] are
   evaluated on every non-exceptional edge out of the function.

   FIXME outlining contract checks into separate functions was motivated
   partly by wanting to call the postcondition function at each return
   statement, which we no longer do; at this point outlining doesn't seem to
   have any advantage over emitting the contracts directly in the function
   body.

   More helpful for optimization might be to make the contracts a wrapper
   function that could be inlined into the caller, the callee, or both.  */

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

const int max_custom_roles = 32;
static contract_role contract_build_roles[max_custom_roles] = {
};

bool valid_configs[CCS_MAYBE + 1][CCS_MAYBE + 1] = {
  { 0, 0, 0, 0, 0, },
  { 0, 1, 0, 0, 0, },
  { 0, 1, 1, 1, 1, },
  { 0, 1, 1, 1, 1, },
  { 0, 1, 0, 0, 1, },
};

void
validate_contract_role (contract_role *role)
{
  gcc_assert (role);
  if (!unchecked_contract_p (role->axiom_semantic))
    error ("axiom contract semantic must be %<assume%> or %<ignore%>");

  if (!valid_configs[role->default_semantic][role->audit_semantic] )
    warning (0, "the %<audit%> semantic should be at least as strong as "
		"the %<default%> semantic");
}

contract_semantic
lookup_concrete_semantic (const char *name)
{
  if (strcmp (name, "ignore") == 0)
    return CCS_IGNORE;
  if (strcmp (name, "assume") == 0)
    return CCS_ASSUME;
  if (strcmp (name, "check_never_continue") == 0
      || strcmp (name, "never") == 0
      || strcmp (name, "abort") == 0)
    return CCS_NEVER;
  if (strcmp (name, "check_maybe_continue") == 0
      || strcmp (name, "maybe") == 0)
    return CCS_MAYBE;
  error ("'%s' is not a valid explicit concrete semantic", name);
  return CCS_INVALID;
}

/* Compare role and name up to either the NUL terminator or the first
   occurrence of colon.  */

static bool
role_name_equal (const char *role, const char *name)
{
  size_t role_len = strcspn (role, ":");
  size_t name_len = strcspn (name, ":");
  if (role_len != name_len)
    return false;
  return strncmp (role, name, role_len) == 0;
}

static bool
role_name_equal (contract_role *role, const char *name)
{
  if (role->name == NULL)
    return false;
  return role_name_equal (role->name, name);
}

contract_role *
get_contract_role (const char *name)
{
  for (int i = 0; i < max_custom_roles; ++i)
    {
      contract_role *potential = contract_build_roles + i;
      if (role_name_equal (potential, name))
	return potential;
    }
  if (role_name_equal (name, "default") || role_name_equal (name, "review"))
    {
      setup_default_contract_role (false);
      return get_contract_role (name);
    }
  return NULL;
}

contract_role *
add_contract_role (const char *name,
		   contract_semantic des,
		   contract_semantic aus,
		   contract_semantic axs,
		   bool update)
{
  for (int i = 0; i < max_custom_roles; ++i)
    {
      contract_role *potential = contract_build_roles + i;
      if (potential->name != NULL
	  && !role_name_equal (potential, name))
	continue;
      if (potential->name != NULL && !update)
	return potential;
      potential->name = name;
      potential->default_semantic = des;
      potential->audit_semantic = aus;
      potential->axiom_semantic = axs;
      return potential;
    }
  return NULL;
}

enum contract_build_level { OFF, DEFAULT, AUDIT };
static bool flag_contract_continuation_mode = false;
static bool flag_contract_assumption_mode = true;
static int flag_contract_build_level = DEFAULT;

static bool contracts_p1332_default = false, contracts_p1332_review = false,
  contracts_std = false, contracts_p1429 = false;

static contract_semantic
get_concrete_check ()
{
  return flag_contract_continuation_mode ? CCS_MAYBE : CCS_NEVER;
}

static contract_semantic
get_concrete_axiom_semantic ()
{
  return flag_contract_assumption_mode ? CCS_ASSUME : CCS_IGNORE;
}

void
setup_default_contract_role (bool update)
{
  contract_semantic check = get_concrete_check ();
  contract_semantic axiom = get_concrete_axiom_semantic ();
  switch (flag_contract_build_level)
    {
      case OFF:
	add_contract_role ("default", CCS_IGNORE, CCS_IGNORE, axiom, update);
	add_contract_role ("review", CCS_IGNORE, CCS_IGNORE, CCS_IGNORE, update);
	break;
      case DEFAULT:
	add_contract_role ("default", check, CCS_IGNORE, axiom, update);
	add_contract_role ("review", check, CCS_IGNORE, CCS_IGNORE, update);
	break;
      case AUDIT:
	add_contract_role ("default", check, check, axiom, update);
	add_contract_role ("review", check, check, CCS_IGNORE, update);
	break;
    }
}

contract_semantic
map_contract_semantic (const char *ident)
{
  if (strcmp (ident, "ignore") == 0)
    return CCS_IGNORE;
  else if (strcmp (ident, "assume") == 0)
    return CCS_ASSUME;
  else if (strcmp (ident, "check_never_continue") == 0)
    return CCS_NEVER;
  else if (strcmp (ident, "check_maybe_continue") == 0)
    return CCS_MAYBE;
  return CCS_INVALID;
}

contract_level
map_contract_level (const char *ident)
{
  if (strcmp (ident, "default") == 0)
    return CONTRACT_DEFAULT;
  else if (strcmp (ident, "audit") == 0)
    return CONTRACT_AUDIT;
  else if (strcmp (ident, "axiom") == 0)
    return CONTRACT_AXIOM;
  return CONTRACT_INVALID;
}


void
handle_OPT_fcontract_build_level_ (const char *arg)
{
  if (contracts_p1332_default || contracts_p1332_review || contracts_p1429)
    {
      error ("%<-fcontract-build-level=%> cannot be mixed with p1332/p1429");
      return;
    }
  else
    contracts_std = true;

  if (strcmp (arg, "off") == 0)
    flag_contract_build_level = OFF;
  else if (strcmp (arg, "default") == 0)
    flag_contract_build_level = DEFAULT;
  else if (strcmp (arg, "audit") == 0)
    flag_contract_build_level = AUDIT;
  else
    error ("%<-fcontract-build-level=%> must be off|default|audit");

  setup_default_contract_role ();
}

void
handle_OPT_fcontract_assumption_mode_ (const char *arg)
{
  if (contracts_p1332_default || contracts_p1332_review || contracts_p1429)
    {
      error ("%<-fcontract-assumption-mode=%> cannot be mixed with p1332/p1429");
      return;
    }
  else
    contracts_std = true;

  if (strcmp (arg, "on") == 0)
    flag_contract_assumption_mode = true;
  else if (strcmp (arg, "off") == 0)
    flag_contract_assumption_mode = false;
  else
    error ("%<-fcontract-assumption-mode=%> must be %<on%> or %<off%>");

  setup_default_contract_role ();
}

void
handle_OPT_fcontract_continuation_mode_ (const char *arg)
{
  if (contracts_p1332_default || contracts_p1332_review || contracts_p1429)
    {
      error ("%<-fcontract-continuation-mode=%> cannot be mixed with p1332/p1429");
      return;
    }
  else
    contracts_std = true;

  if (strcmp (arg, "on") == 0)
    flag_contract_continuation_mode = true;
  else if (strcmp (arg, "off") == 0)
    flag_contract_continuation_mode = false;
  else
    error ("%<-fcontract-continuation-mode=%> must be %<on%> or %<off%>");

  setup_default_contract_role ();
}

void
handle_OPT_fcontract_role_ (const char *arg)
{
  const char *name = arg;
  const char *vals = strchr (name, ':');
  if (vals == NULL)
    {
      error ("%<-fcontract-role=%> must be in the form role:semantics");
      return;
    }

  contract_semantic dess = CCS_INVALID, auss = CCS_INVALID, axss = CCS_INVALID;
  char *des = NULL, *aus = NULL, *axs = NULL;
  des = xstrdup (vals + 1);

  aus = strchr (des, ',');
  if (aus == NULL)
    {
      error ("%<-fcontract-role=%> semantics must include default,audit,axiom values");
      goto validate;
    }
  *aus = '\0'; // null terminate des
  aus = aus + 1; // move past null

  axs = strchr (aus, ',');
  if (axs == NULL)
    {
      error ("%<-fcontract-role=%> semantics must include default,audit,axiom values");
      goto validate;
    }
  *axs = '\0'; // null terminate aus
  axs = axs + 1; // move past null

  dess = lookup_concrete_semantic (des);
  auss = lookup_concrete_semantic (aus);
  axss = lookup_concrete_semantic (axs);
validate:
  free (des);
  if (dess == CCS_INVALID || auss == CCS_INVALID || axss == CCS_INVALID)
    return;

  bool is_defalult_role = role_name_equal (name, "default");
  bool is_review_role = role_name_equal (name, "review");
  bool is_std_role = is_defalult_role || is_review_role;
  if ((contracts_std && is_std_role) || (contracts_p1429 && is_defalult_role))
    {
      error ("%<-fcontract-role=%> cannot be mixed with std/p1429 contract flags");
      return;
    }
  else if (is_std_role)
    {
      contracts_p1332_default |= is_defalult_role;
      contracts_p1332_review |= is_review_role;
    }

  contract_role *role = add_contract_role (name, dess, auss, axss);

  if (role == NULL)
    {
      // TODO: not enough space?
      error ("%<-fcontract-level=%> too many custom roles");
      return;
    }
  else
    validate_contract_role (role);
}

void
handle_OPT_fcontract_semantic_ (const char *arg)
{
  if (!strchr (arg, ':'))
    {
      error ("%<-fcontract-semantic=%> must be in the form level:semantic");
      return;
    }

  if (contracts_std || contracts_p1332_default)
    {
      error ("%<-fcontract-semantic=%> cannot be mixed with std/p1332 contract flags");
      return;
    }
  contracts_p1429 = true;

  contract_role *role = get_contract_role ("default");
  if (!role)
    {
      error ("%<-fcontract-semantic=%> cannot find default role");
      return;
    }

  const char *semantic = strchr (arg, ':') + 1;
  contract_semantic sem = lookup_concrete_semantic (semantic);
  if (sem == CCS_INVALID)
    return;

  if (strncmp ("default:", arg, 8) == 0)
    role->default_semantic = sem;
  else if (strncmp ("audit:", arg, 6) == 0)
    role->audit_semantic = sem;
  else if (strncmp ("axiom:", arg, 6) == 0)
    role->axiom_semantic = sem;
  else
    error ("%<-fcontract-semantic=%> level must be default, audit, or axiom");
  validate_contract_role (role);
}

/* Convert a contract CONFIG into a contract_mode.  */

static contract_mode
contract_config_to_mode (tree config)
{
  if (config == NULL_TREE)
    return contract_mode (CONTRACT_DEFAULT, get_default_contract_role ());

  /* TREE_LIST has TREE_VALUE is a level and TREE_PURPOSE is role.  */
  if (TREE_CODE (config) == TREE_LIST)
    {
      contract_role *role = NULL;
      if (TREE_PURPOSE (config))
	role = get_contract_role (IDENTIFIER_POINTER (TREE_PURPOSE (config)));
      if (!role)
	role = get_default_contract_role ();

      contract_level level =
	map_contract_level (IDENTIFIER_POINTER (TREE_VALUE (config)));
      return contract_mode (level, role);
    }

  /* Literal semantic.  */
  gcc_assert (TREE_CODE (config) == IDENTIFIER_NODE);
  contract_semantic semantic =
    map_contract_semantic (IDENTIFIER_POINTER (config));
  return contract_mode (semantic);
}

/* Convert a contract's config into a concrete semantic using the current
   contract semantic mapping.  */

static contract_semantic
compute_concrete_semantic (tree contract)
{
  contract_mode mode = contract_config_to_mode (CONTRACT_MODE (contract));
  /* Compute the concrete semantic for the contract.  */
  if (!flag_contract_mode)
    /* If contracts are off, treat all contracts as ignore.  */
    return CCS_IGNORE;
  else if (mode.kind == contract_mode::cm_invalid)
    return CCS_INVALID;
  else if (mode.kind == contract_mode::cm_explicit)
    return mode.get_semantic ();
  else
    {
      gcc_assert (mode.get_role ());
      gcc_assert (mode.get_level () != CONTRACT_INVALID);
      contract_level level = mode.get_level ();
      contract_role *role = mode.get_role ();
      if (level == CONTRACT_DEFAULT)
	return role->default_semantic;
      else if (level == CONTRACT_AUDIT)
	return role->audit_semantic;
      else if (level == CONTRACT_AXIOM)
	return role->axiom_semantic;
    }
  gcc_assert (false);
}

/* Return true if any contract in CONTRACT_ATTRs is not yet parsed.  */

bool
contract_any_deferred_p (tree contract_attr)
{
  for (; contract_attr; contract_attr = CONTRACT_CHAIN (contract_attr))
    if (CONTRACT_CONDITION_DEFERRED_P (CONTRACT_STATEMENT (contract_attr)))
      return true;
  return false;
}

/* Returns true if all attributes are contracts.  */

bool
all_attributes_are_contracts_p (tree attributes)
{
  for (; attributes; attributes = TREE_CHAIN (attributes))
    if (!cxx_contract_attribute_p (attributes))
      return false;
  return true;
}

/* Mark most of a contract as being invalid.  */

tree
invalidate_contract (tree t)
{
  if (TREE_CODE (t) == POSTCONDITION_STMT && POSTCONDITION_IDENTIFIER (t))
    POSTCONDITION_IDENTIFIER (t) = error_mark_node;
  CONTRACT_CONDITION (t) = error_mark_node;
  CONTRACT_COMMENT (t) = error_mark_node;
  return t;
}

/* Returns an invented parameter declration of the form 'TYPE ID' for the
   purpose of parsing the postcondition.

   We use a PARM_DECL instead of a VAR_DECL so that tsubst forces a lookup
   in local specializations when we instantiate these things later.  */

tree
make_postcondition_variable (cp_expr id, tree type)
{
  if (id == error_mark_node)
    return id;

  tree decl = build_lang_decl (PARM_DECL, id, type);
  DECL_ARTIFICIAL (decl) = true;
  DECL_SOURCE_LOCATION (decl) = id.get_location ();

  pushdecl (decl);
  return decl;
}

/* As above, except that the type is unknown.  */

tree
make_postcondition_variable (cp_expr id)
{
  return make_postcondition_variable (id, make_auto ());
}

/* Check that the TYPE is valid for a named postcondition variable. Emit a
   diagnostic if it is not.  Returns TRUE if the result is OK and false
   otherwise.  */

bool
check_postcondition_result (tree decl, tree type, location_t loc)
{
  /* Do not be confused by targetm.cxx.cdtor_return_this ();
     conceptually, cdtors have no return value.  */
  if (VOID_TYPE_P (type)
      || DECL_CONSTRUCTOR_P (decl)
      || DECL_DESTRUCTOR_P (decl))
    {
      error_at (loc,
		DECL_CONSTRUCTOR_P (decl)
		? G_("constructor does not return a value to test")
		: DECL_DESTRUCTOR_P (decl)
		? G_("destructor does not return a value to test")
		: G_("function does not return a value to test"));
      return false;
    }

  return true;
}

/* Instantiate each postcondition with the return type to finalize the
   attribute.  */

void
rebuild_postconditions (tree decl)
{
  tree type = TREE_TYPE (TREE_TYPE (decl));
  tree attributes = DECL_CONTRACTS (decl);

  for (; attributes ; attributes = TREE_CHAIN (attributes))
    {
      if (!cxx_contract_attribute_p (attributes))
	continue;
      tree contract = TREE_VALUE (TREE_VALUE (attributes));
      if (TREE_CODE (contract) != POSTCONDITION_STMT)
	continue;
      tree condition = CONTRACT_CONDITION (contract);

      /* If any conditions are deferred, they're all deferred.  Note that
	 we don't have to instantiate postconditions in that case because
	 the type is available through the declaration.  */
      if (TREE_CODE (condition) == DEFERRED_PARSE)
	return;

      tree oldvar = POSTCONDITION_IDENTIFIER (contract);
      if (!oldvar)
	continue;

      /* Always update the context of the result variable so that it can
	 be remapped by remap_contracts.  */
      DECL_CONTEXT (oldvar) = decl;

      /* If the return type is undeduced, defer until later.  */
      if (TREE_CODE (type) == TEMPLATE_TYPE_PARM)
	return;

      /* Check the postcondition variable.  */
      location_t loc = DECL_SOURCE_LOCATION (oldvar);
      if (!check_postcondition_result (decl, type, loc))
	{
	  invalidate_contract (contract);
	  continue;
	}

      /* "Instantiate" the result variable using the known type.  Also update
	  the context so the inliner will actually remap this the parameter when
	  generating contract checks.  */
      tree newvar = copy_node (oldvar);
      TREE_TYPE (newvar) = type;

      /* Make parameters and result available for substitution.  */
      local_specialization_stack stack (lss_copy);
      for (tree t = DECL_ARGUMENTS (decl); t != NULL_TREE; t = TREE_CHAIN (t))
	register_local_identity (t);
      register_local_specialization (newvar, oldvar);

      ++processing_contract_condition;
      condition = tsubst_expr (condition, make_tree_vec (0),
			       tf_warning_or_error, decl);
      --processing_contract_condition;

      /* Update the contract condition and result.  */
      POSTCONDITION_IDENTIFIER (contract) = newvar;
      CONTRACT_CONDITION (contract) = finish_contract_condition (condition);
    }
}

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
      /* FIXME cases where we end up here
	 #line macro usage (oof)
	 contracts10.C
	 contracts11.C  */
      const char *str = expr_to_string (condition);
      return build_string_literal (strlen (str) + 1, str);
    }

  tree t = build_string_literal (strlen (str) + 1, str);
  free (str);
  return t;
}

/* Build a contract statement.  */

tree
grok_contract (tree attribute, tree mode, tree result, cp_expr condition,
	       location_t loc)
{
  if (condition == error_mark_node)
    return error_mark_node;

  tree_code code;
  if (is_attribute_p ("assert", attribute))
    code = ASSERTION_STMT;
  else if (is_attribute_p ("pre", attribute))
    code = PRECONDITION_STMT;
  else if (is_attribute_p ("post", attribute))
    code = POSTCONDITION_STMT;
  else
    gcc_unreachable ();

  /* Build the contract. The condition is added later.  In the case that
     the contract is deferred, result an plain identifier, not a result
     variable.  */
  tree contract;
  tree type = void_type_node;
  if (code != POSTCONDITION_STMT)
    contract = build3_loc (loc, code, type, mode, NULL_TREE, NULL_TREE);
  else
    contract = build4_loc (loc, code, type, mode, NULL_TREE, NULL_TREE, result);

  /* Determine the concrete semantic.  */
  set_contract_semantic (contract, compute_concrete_semantic (contract));

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

/* Build the contract attribute specifier where IDENTIFIER is one of 'pre',
   'post' or 'assert' and CONTRACT is the underlying statement.  */
tree
finish_contract_attribute (tree identifier, tree contract)
{
  if (contract == error_mark_node)
    return error_mark_node;

  tree attribute = build_tree_list (build_tree_list (NULL_TREE, identifier),
				    build_tree_list (NULL_TREE, contract));

  /* Mark the attribute as dependent if the condition is dependent.

     TODO: I'm not sure this is strictly necessary. It's going to be marked as
     such by a subroutine of cplus_decl_attributes. */
  tree condition = CONTRACT_CONDITION (contract);
  if (TREE_CODE (condition) == DEFERRED_PARSE
      || value_dependent_expression_p (condition))
    ATTR_IS_DEPENDENT (attribute) = true;

  return attribute;
}

/* Update condition of a late-parsed contract and postcondition variable,
   if any.  */

void
update_late_contract (tree contract, tree result, tree condition)
{
  if (TREE_CODE (contract) == POSTCONDITION_STMT)
    POSTCONDITION_IDENTIFIER (contract) = result;

  /* Generate the comment from the original condition.  */
  CONTRACT_COMMENT (contract) = build_comment (condition);

  /* The condition is converted to bool.  */
  condition = finish_contract_condition (condition);
  CONTRACT_CONDITION (contract) = condition;
}

/* Return TRUE iff ATTR has been parsed by the front-end as a c++2a contract
   attribute. */

bool
cxx_contract_attribute_p (const_tree attr)
{
  if (attr == NULL_TREE
      || TREE_CODE (attr) != TREE_LIST)
    return false;

  if (!TREE_PURPOSE (attr) || TREE_CODE (TREE_PURPOSE (attr)) != TREE_LIST)
    return false;
  if (!TREE_VALUE (attr) || TREE_CODE (TREE_VALUE (attr)) != TREE_LIST)
    return false;
  if (!TREE_VALUE (TREE_VALUE (attr)))
    return false;

  return (TREE_CODE (TREE_VALUE (TREE_VALUE (attr))) == PRECONDITION_STMT
      || TREE_CODE (TREE_VALUE (TREE_VALUE (attr))) == POSTCONDITION_STMT
      || TREE_CODE (TREE_VALUE (TREE_VALUE (attr))) == ASSERTION_STMT);
}

/* True if ATTR is an assertion.  */

bool
cp_contract_assertion_p (const_tree attr)
{
  /* This is only an assertion if it is a valid cxx contract attribute and the
     statement is an ASSERTION_STMT.  */
  return cxx_contract_attribute_p (attr)
    && TREE_CODE (CONTRACT_STATEMENT (attr)) == ASSERTION_STMT;
}

/* Remove all c++2a style contract attributes from the DECL_ATTRIBUTEs of the
   FUNCTION_DECL FNDECL.  */

void
remove_contract_attributes (tree fndecl)
{
  tree list = NULL_TREE;
  for (tree p = DECL_ATTRIBUTES (fndecl); p; p = TREE_CHAIN (p))
    if (!cxx_contract_attribute_p (p))
      list = tree_cons (TREE_PURPOSE (p), TREE_VALUE (p), list);
  DECL_ATTRIBUTES (fndecl) = nreverse (list);
}

static tree find_first_non_contract (tree attributes)
{
  tree head = attributes;
  tree p = find_contract (attributes);

  /* There are no contracts.  */
  if (!p)
    return head;

  /* There are leading contracts.  */
  if (p == head)
    {
      while (cxx_contract_attribute_p (p))
	p = TREE_CHAIN (p);
      head = p;
    }

  return head;
}

/* Remove contracts from ATTRIBUTES.  */

tree splice_out_contracts (tree attributes)
{
  tree head = find_first_non_contract (attributes);
  if (!head)
    return NULL_TREE;

  /* Splice out remaining contracts.  */
  tree p = TREE_CHAIN (head);
  tree q = head;
  while (p)
    {
      if (cxx_contract_attribute_p (p))
	{
	  /* Skip a sequence of contracts and then link q to the next
	     non-contract attribute.  */
	  do
	    p = TREE_CHAIN (p);
	  while (cxx_contract_attribute_p (p));
	  TREE_CHAIN (q) = p;
	}
      else
	p = TREE_CHAIN (p);
    }

  return head;
}

/* Copy contract attributes from NEWDECL onto the attribute list of OLDDECL.  */

void copy_contract_attributes (tree olddecl, tree newdecl)
{
  tree attrs = NULL_TREE;
  for (tree c = DECL_CONTRACTS (newdecl); c; c = TREE_CHAIN (c))
    {
      if (!cxx_contract_attribute_p (c))
	continue;
      attrs = tree_cons (TREE_PURPOSE (c), TREE_VALUE (c), attrs);
    }
  attrs = chainon (DECL_ATTRIBUTES (olddecl), nreverse (attrs));
  DECL_ATTRIBUTES (olddecl) = attrs;

  /* And update DECL_CONTEXT of the postcondition result identifier.  */
  rebuild_postconditions (olddecl);
}

/* Returns the parameter corresponding to the return value of a guarded
   function D.  Returns NULL_TREE if D has no postconditions or is void.  */

static tree
get_postcondition_result_parameter (tree d)
{
  if (!d || d == error_mark_node)
    return NULL_TREE;

  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (d))))
    return NULL_TREE;

  tree post = DECL_POST_FN (d);
  if (!post || post == error_mark_node)
    return NULL_TREE;

  for (tree arg = DECL_ARGUMENTS (post); arg; arg = TREE_CHAIN (arg))
    if (!TREE_CHAIN (arg))
      return arg;

  return NULL_TREE;
}


/* For use with the tree inliner. This preserves non-mapped local variables,
   such as postcondition result variables, during remapping.  */

static tree
retain_decl (tree decl, copy_body_data *)
{
  return decl;
}

/* Rewrite the condition of contract in place, so that references to SRC's
   parameters are updated to refer to DST's parameters. The postcondition
   result variable is left unchanged.

   This, along with remap_contracts, are subroutines of duplicate_decls.
   When declarations are merged, we sometimes need to update contracts to
   refer to new parameters.

   If DUPLICATE_P is true, this is called by duplicate_decls to rewrite contacts
   in terms of a new set of parameters. In this case, we can retain local
   variables appearing in the contract because the contract is not being
   prepared for insertion into a new function. Importantly, this preserves the
   references to postcondition results, which are not replaced during merging.

   If false, we're preparing to emit the contract condition into the body
   of a new function, so we need to make copies of all local variables
   appearing in the contract (e.g., if it includes a lambda expression). Note
   that in this case, postcondition results are mapped to the last parameter
   of DST.

   This is also used to reuse a parent type's contracts on virtual methods.  */

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
  if (TREE_CODE (src) == FUNCTION_DECL)
    src = DECL_ARGUMENTS (src);
  if (TREE_CODE (dst) == FUNCTION_DECL)
    dst = DECL_ARGUMENTS (dst);

  for (tree sp = src, dp = dst;
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
    }
  if (!do_remap)
    return;

  walk_tree (&CONTRACT_CONDITION (contract), copy_tree_body_r, &id, NULL);
}

/* Rewrite any references to SRC's PARM_DECLs to the corresponding PARM_DECL in
   DST in all of the contract attributes in CONTRACTS by calling remap_contract
   on each.

   This is used for two purposes: to rewrite contract attributes during
   duplicate_decls, and to prepare contracts for emission into a function's
   respective precondition and postcondition functions. DUPLICATE_P is used
   to determine the context in which this function is called. See above for
   the behavior described by this flag.  */

void
remap_contracts (tree src, tree dst, tree contracts, bool duplicate_p)
{
  for (tree attr = contracts; attr; attr = CONTRACT_CHAIN (attr))
    {
      if (!cxx_contract_attribute_p (attr))
	continue;
      tree contract = CONTRACT_STATEMENT (attr);
      if (TREE_CODE (CONTRACT_CONDITION (contract)) != DEFERRED_PARSE)
	remap_contract (src, dst, contract, duplicate_p);
    }
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
   the first argument of the FUNCTION_DECL FN.  */

static void
remap_dummy_this (tree fn, tree *expr)
{
  walk_tree (expr, remap_dummy_this_1, fn, NULL);
}

/* Contract matching.  */

/* True if the contract is valid.  */

static bool
contract_valid_p (tree contract)
{
  return CONTRACT_CONDITION (contract) != error_mark_node;
}

/* True if the contract attribute is valid.  */

static bool
contract_attribute_valid_p (tree attribute)
{
  return contract_valid_p (TREE_VALUE (TREE_VALUE (attribute)));
}

/* Compare the contract conditions of OLD_ATTR and NEW_ATTR. Returns false
   if the conditions are equivalent, and true otherwise.  */

static bool
check_for_mismatched_contracts (tree old_attr, tree new_attr,
			       contract_matching_context ctx)
{
  tree old_contract = CONTRACT_STATEMENT (old_attr);
  tree new_contract = CONTRACT_STATEMENT (new_attr);

  /* Different kinds of contracts do not match.  */
  if (TREE_CODE (old_contract) != TREE_CODE (new_contract))
    {
      auto_diagnostic_group d;
      error_at (EXPR_LOCATION (new_contract),
		ctx == cmc_declaration
		? "mismatched contract attribute in declaration"
		: "mismatched contract attribute in override");
      inform (EXPR_LOCATION (old_contract), "previous contract here");
      return true;
    }

  /* A deferred contract tentatively matches.  */
  if (CONTRACT_CONDITION_DEFERRED_P (new_contract))
    return false;

  /* Compare the conditions of the contracts.  We fold immediately to avoid
     issues comparing contracts on overrides that use parameters -- see
     contracts-pre3.  */
  tree t1 = cp_fully_fold_init (CONTRACT_CONDITION (old_contract));
  tree t2 = cp_fully_fold_init (CONTRACT_CONDITION (new_contract));

  /* Compare the contracts. The fold doesn't eliminate conversions to members.
     Set the comparing_override_contracts flag to ensure that references
     through 'this' are equal if they designate the same member, regardless of
     the path those members.  */
  bool saved_comparing_contracts = comparing_override_contracts;
  comparing_override_contracts = (ctx == cmc_override);
  bool matching_p = cp_tree_equal (t1, t2);
  comparing_override_contracts = saved_comparing_contracts;

  if (!matching_p)
    {
      auto_diagnostic_group d;
      error_at (EXPR_LOCATION (CONTRACT_CONDITION (new_contract)),
		ctx == cmc_declaration
		? "mismatched contract condition in declaration"
		: "mismatched contract condition in override");
      inform (EXPR_LOCATION (CONTRACT_CONDITION (old_contract)),
	      "previous contract here");
      return true;
    }

  return false;
}

/* Compare the contract attributes of OLDDECL and NEWDECL. Returns true
   if the contracts match, and false if they differ.  */

bool
match_contract_conditions (location_t oldloc, tree old_attrs,
			   location_t newloc, tree new_attrs,
			   contract_matching_context ctx)
{
  /* Contracts only match if they are both specified.  */
  if (!old_attrs || !new_attrs)
    return true;

  /* Compare each contract in turn.  */
  while (old_attrs && new_attrs)
    {
      /* If either contract is ill-formed, skip the rest of the comparison,
	 since we've already diagnosed an error.  */
      if (!contract_attribute_valid_p (new_attrs)
	  || !contract_attribute_valid_p (old_attrs))
	return false;

      if (check_for_mismatched_contracts (old_attrs, new_attrs, ctx))
	return false;
      old_attrs = CONTRACT_CHAIN (old_attrs);
      new_attrs = CONTRACT_CHAIN (new_attrs);
    }

  /* If we didn't compare all attributes, the contracts don't match.  */
  if (old_attrs || new_attrs)
    {
      auto_diagnostic_group d;
      error_at (newloc,
		ctx == cmc_declaration
		? "declaration has a different number of contracts than "
		  "previously declared"
		: "override has a different number of contracts than "
		  "previously declared");
      inform (oldloc,
	      new_attrs
	      ? "original declaration with fewer contracts here"
	      : "original declaration with more contracts here");
      return false;
    }

  return true;
}

/* Deferred contract mapping.

   This is used to compare late-parsed contracts on overrides with their
   base class functions.

   TODO: It seems like this could be replaced by a simple list that maps from
   overrides to their base functions. It's not clear that we really need
   a map to a function + a list of contracts.   */

/* Map from FNDECL to a tree list of contracts that have not been matched or
   diagnosed yet.  The TREE_PURPOSE is the basefn we're overriding, and the
   TREE_VALUE is the list of contract attrs for BASEFN.  */

static hash_map<tree_decl_hash, tree> pending_guarded_decls;

void
defer_guarded_contract_match (tree fndecl, tree fn, tree contracts)
{
  if (!pending_guarded_decls.get (fndecl))
    {
      pending_guarded_decls.put (fndecl, build_tree_list (fn, contracts));
      return;
    }
  for (tree pending = *pending_guarded_decls.get (fndecl);
      pending;
      pending = TREE_CHAIN (pending))
    {
      if (TREE_VALUE (pending) == contracts)
	return;
      if (TREE_CHAIN (pending) == NULL_TREE)
	TREE_CHAIN (pending) = build_tree_list (fn, contracts);
    }
}

/* If the FUNCTION_DECL DECL has any contracts that had their matching
   deferred earlier, do that checking now.  */

void
match_deferred_contracts (tree decl)
{
  tree *tp = pending_guarded_decls.get (decl);
  if (!tp)
    return;

  gcc_assert(!contract_any_deferred_p (DECL_CONTRACTS (decl)));

  processing_template_decl_sentinel ptds;
  processing_template_decl = uses_template_parms (decl);

  /* Do late contract matching.  */
  for (tree pending = *tp; pending; pending = TREE_CHAIN (pending))
    {
      tree new_contracts = TREE_VALUE (pending);
      location_t new_loc = CONTRACT_SOURCE_LOCATION (new_contracts);
      tree old_contracts = DECL_CONTRACTS (decl);
      location_t old_loc = CONTRACT_SOURCE_LOCATION (old_contracts);
      tree base = TREE_PURPOSE (pending);
      match_contract_conditions (new_loc, new_contracts,
				 old_loc, old_contracts,
				 base ? cmc_override : cmc_declaration);
    }

  /* Clear out deferred match list so we don't check it twice.  */
  pending_guarded_decls.remove (decl);
}

/* Map from FUNCTION_DECL to a FUNCTION_DECL for either the PRE_FN or POST_FN.
   These are used to parse contract conditions and are called inside the body
   of the guarded function.  */
static GTY(()) hash_map<tree, tree> *decl_pre_fn;
static GTY(()) hash_map<tree, tree> *decl_post_fn;

/* Returns the precondition funtion for D, or null if not set.  */

tree
get_precondition_function (tree d)
{
  hash_map_maybe_create<hm_ggc> (decl_pre_fn);
  tree *result = decl_pre_fn->get (d);
  return result ? *result : NULL_TREE;
}

/* Returns the postcondition funtion for D, or null if not set.  */

tree
get_postcondition_function (tree d)
{
  hash_map_maybe_create<hm_ggc> (decl_post_fn);
  tree *result = decl_post_fn->get (d);
  return result ? *result : NULL_TREE;
}

/* Makes PRE the precondition function for D.  */

void
set_precondition_function (tree d, tree pre)
{
  gcc_assert (pre);
  hash_map_maybe_create<hm_ggc> (decl_pre_fn);
  gcc_assert (!decl_pre_fn->get (d));
  decl_pre_fn->put (d, pre);
}

/* Makes POST the postcondition function for D.  */

void
set_postcondition_function (tree d, tree post)
{
  gcc_assert (post);
  hash_map_maybe_create<hm_ggc> (decl_post_fn);
  gcc_assert (!decl_post_fn->get (d));
  decl_post_fn->put (d, post);
}

/* Set the PRE and POST functions for D.  Note that PRE and POST can be
   null in this case. If so the functions are not recorded.  */

void
set_contract_functions (tree d, tree pre, tree post)
{
  if (pre)
    set_precondition_function (d, pre);
  if (post)
    set_postcondition_function (d, post);
}

/* Return a copy of the FUNCTION_DECL IDECL with its own unshared
   PARM_DECL and DECL_ATTRIBUTEs.  */

static tree
copy_fn_decl (tree idecl)
{
  tree decl = copy_decl (idecl);
  DECL_ATTRIBUTES (decl) = copy_list (DECL_ATTRIBUTES (idecl));

  if (DECL_RESULT (idecl))
    {
      DECL_RESULT (decl) = copy_decl (DECL_RESULT (idecl));
      DECL_CONTEXT (DECL_RESULT (decl)) = decl;
    }
  if (!DECL_ARGUMENTS (idecl) || VOID_TYPE_P (DECL_ARGUMENTS (idecl)))
    return decl;

  tree last = DECL_ARGUMENTS (decl) = copy_decl (DECL_ARGUMENTS (decl));
  DECL_CONTEXT (last) = decl;
  for (tree p = TREE_CHAIN (DECL_ARGUMENTS (idecl)); p; p = TREE_CHAIN (p))
    {
      if (VOID_TYPE_P (p))
	{
	  TREE_CHAIN (last) = void_list_node;
	  break;
	}
      last = TREE_CHAIN (last) = copy_decl (p);
      DECL_CONTEXT (last) = decl;
    }
  return decl;
}

/* Build a declaration for the pre- or postcondition of a guarded FNDECL.  */

static tree
build_contract_condition_function (tree fndecl, bool pre)
{
  if (TREE_TYPE (fndecl) == error_mark_node)
    return error_mark_node;
  if (DECL_IOBJ_MEMBER_FUNCTION_P (fndecl)
      && !TYPE_METHOD_BASETYPE (TREE_TYPE (fndecl)))
    return error_mark_node;

  /* Create and rename the unchecked function and give an internal name.  */
  tree fn = copy_fn_decl (fndecl);
  DECL_RESULT (fn) = NULL_TREE;
  tree value_type = pre ? void_type_node : TREE_TYPE (TREE_TYPE (fn));

  /* Don't propagate declaration attributes to the checking function,
     including the original contracts.  */
  DECL_ATTRIBUTES (fn) = NULL_TREE;

  tree arg_types = NULL_TREE;
  tree *last = &arg_types;

  /* FIXME will later optimizations delete unused args to prevent extra arg
     passing? do we care? */
  tree class_type = NULL_TREE;
  for (tree arg_type = TYPE_ARG_TYPES (TREE_TYPE (fn));
      arg_type && arg_type != void_list_node;
      arg_type = TREE_CHAIN (arg_type))
    {
      if (DECL_IOBJ_MEMBER_FUNCTION_P (fndecl)
	  && TYPE_ARG_TYPES (TREE_TYPE (fn)) == arg_type)
      {
	class_type = TREE_TYPE (TREE_VALUE (arg_type));
	continue;
      }
      *last = build_tree_list (TREE_PURPOSE (arg_type), TREE_VALUE (arg_type));
      last = &TREE_CHAIN (*last);
    }

  if (pre || VOID_TYPE_P (value_type))
    *last = void_list_node;
  else
    {
      tree name = get_identifier ("__r");
      tree parm = build_lang_decl (PARM_DECL, name, value_type);
      DECL_CONTEXT (parm) = fn;
      DECL_ARTIFICIAL (parm) = true;
      DECL_ARGUMENTS (fn) = chainon (DECL_ARGUMENTS (fn), parm);

      *last = build_tree_list (NULL_TREE, value_type);
      TREE_CHAIN (*last) = void_list_node;

      /* The handler is a void return.  */
      value_type = void_type_node;
    }

  TREE_TYPE (fn) = build_function_type (value_type, arg_types);
  if (DECL_IOBJ_MEMBER_FUNCTION_P (fndecl))
    TREE_TYPE (fn) = build_method_type (class_type, TREE_TYPE (fn));

  DECL_NAME (fn) = copy_node (DECL_NAME (fn));
  DECL_INITIAL (fn) = error_mark_node;
  DECL_ABSTRACT_ORIGIN (fn) = fndecl;

  IDENTIFIER_VIRTUAL_P (DECL_NAME (fn)) = false;
  DECL_VIRTUAL_P (fn) = false;

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

      DECL_INTERFACE_KNOWN (fn) = true;
    }

  DECL_ARTIFICIAL (fn) = true;

  /* Update various inline related declaration properties.  */
  //DECL_DECLARED_INLINE_P (fn) = true;
  DECL_DISREGARD_INLINE_LIMITS (fn) = true;
  TREE_NO_WARNING (fn) = 1;

  return fn;
}

/* Return true if CONTRACT is checked or assumed under the current build
   configuration. */

bool
contract_active_p (tree contract)
{
  return get_contract_semantic (contract) != CCS_IGNORE;
}

static bool
has_active_contract_condition (tree d, tree_code c)
{
  for (tree as = DECL_CONTRACTS (d) ; as != NULL_TREE; as = CONTRACT_CHAIN (as))
    {
      tree contract = TREE_VALUE (TREE_VALUE (as));
      if (TREE_CODE (contract) == c && contract_active_p (contract))
	return true;
    }
  return false;
}

/* True if D has any checked or assumed preconditions.  */

static bool
has_active_preconditions (tree d)
{
  return has_active_contract_condition (d, PRECONDITION_STMT);
}

/* True if D has any checked or assumed postconditions.  */

static bool
has_active_postconditions (tree d)
{
  return has_active_contract_condition (d, POSTCONDITION_STMT);
}

/* Return true if any contract in the CONTRACT list is checked or assumed
   under the current build configuration. */

bool
contract_any_active_p (tree contract)
{
  for (; contract != NULL_TREE; contract = CONTRACT_CHAIN (contract))
    if (contract_active_p (TREE_VALUE (TREE_VALUE (contract))))
      return true;
  return false;
}

/* Do we need to mess with contracts for DECL1?  */

static bool
handle_contracts_p (tree decl1)
{
  return (flag_contracts
	  && !processing_template_decl
	  && DECL_ABSTRACT_ORIGIN (decl1) == NULL_TREE
	  && contract_any_active_p (DECL_CONTRACTS (decl1)));
}

/* Should we break out DECL1's pre/post contracts into separate functions?
   FIXME I'd like this to default to 0, but that will need an overhaul to the
   return identifier handling to just refer to the RESULT_DECL.  */

static bool
outline_contracts_p (tree decl1)
{
  return (!DECL_CONSTRUCTOR_P (decl1)
	  && !DECL_DESTRUCTOR_P (decl1));
}

/* Build the precondition checking function for D.  */

static tree
build_precondition_function (tree d)
{
  if (!has_active_preconditions (d))
    return NULL_TREE;

  return build_contract_condition_function (d, /*pre=*/true);
}

/* Build the postcondition checking function for D. If the return
   type is undeduced, don't build the function yet. We do that in
   apply_deduced_return_type.  */

static tree
build_postcondition_function (tree d)
{
  if (!has_active_postconditions (d))
    return NULL_TREE;

  tree type = TREE_TYPE (TREE_TYPE (d));
  if (is_auto (type))
    return NULL_TREE;

  return build_contract_condition_function (d, /*pre=*/false);
}

static void
build_contract_function_decls (tree d)
{
  /* Constructors and destructors have their contracts inserted inline.  */
  if (!outline_contracts_p (d))
    return;

  /* Build the pre/post functions (or not).  */
  tree pre = build_precondition_function (d);
  tree post = build_postcondition_function (d);
  set_contract_functions (d, pre, post);
}

static const char *
get_contract_level_name (tree contract)
{
  if (CONTRACT_LITERAL_MODE_P (contract))
    return "";
  if (tree mode = CONTRACT_MODE (contract))
    if (tree level = TREE_VALUE (mode))
      return IDENTIFIER_POINTER (level);
  return "default";
}

static const char *
get_contract_role_name (tree contract)
{
  if (CONTRACT_LITERAL_MODE_P (contract))
    return "";
  if (tree mode = CONTRACT_MODE (contract))
    if (tree role = TREE_PURPOSE (mode))
      return IDENTIFIER_POINTER (role);
  return "default";
}

/* Build a layout-compatible internal version of std::contract_violation.  */

static tree
get_pseudo_contract_violation_type ()
{
  if (!pseudo_contract_violation_type)
    {
      /* Must match <contract>:
	 class contract_violation {
	   const char* _M_file;
	   const char* _M_function;
	   const char* _M_comment;
	   const char* _M_level;
	   const char* _M_role;
	   uint_least32_t _M_line;
	   signed char _M_continue;
	 If this changes, also update the initializer in
	 build_contract_violation.  */
      const tree types[] = { const_string_type_node,
			     const_string_type_node,
			     const_string_type_node,
			     const_string_type_node,
			     const_string_type_node,
			     uint_least32_type_node,
			     signed_char_type_node };
      tree fields = NULL_TREE;
      for (tree type : types)
	{
	  /* finish_builtin_struct wants fieldss chained in reverse.  */
	  tree next = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				  NULL_TREE, type);
	  DECL_CHAIN (next) = fields;
	  fields = next;
	}
      iloc_sentinel ils (input_location);
      input_location = BUILTINS_LOCATION;
      pseudo_contract_violation_type = make_class_type (RECORD_TYPE);
      finish_builtin_struct (pseudo_contract_violation_type,
			     "__pseudo_contract_violation",
			     fields, NULL_TREE);
      CLASSTYPE_AS_BASE (pseudo_contract_violation_type)
	= pseudo_contract_violation_type;
      DECL_CONTEXT (TYPE_NAME (pseudo_contract_violation_type))
	= FROB_CONTEXT (global_namespace);
      TREE_PUBLIC (TYPE_NAME (pseudo_contract_violation_type)) = true;
      CLASSTYPE_LITERAL_P (pseudo_contract_violation_type) = true;
      CLASSTYPE_LAZY_COPY_CTOR (pseudo_contract_violation_type) = true;
      xref_basetypes (pseudo_contract_violation_type, /*bases=*/NULL_TREE);
      pseudo_contract_violation_type
	= cp_build_qualified_type (pseudo_contract_violation_type,
				   TYPE_QUAL_CONST);
    }
  return pseudo_contract_violation_type;
}

/* Return a VAR_DECL to pass to handle_contract_violation.  */

static tree
build_contract_violation (tree contract, contract_continuation cmode)
{
  expanded_location loc = expand_location (EXPR_LOCATION (contract));
  const char *function = fndecl_name (DECL_ORIGIN (current_function_decl));
  const char *level = get_contract_level_name (contract);
  const char *role = get_contract_role_name (contract);

  /* Must match the type layout in get_pseudo_contract_violation_type.  */
  tree ctor = build_constructor_va
    (init_list_type_node, 7,
     NULL_TREE, build_string_literal (loc.file),
     NULL_TREE, build_string_literal (function),
     NULL_TREE, CONTRACT_COMMENT (contract),
     NULL_TREE, build_string_literal (level),
     NULL_TREE, build_string_literal (role),
     NULL_TREE, build_int_cst (uint_least32_type_node, loc.line),
     NULL_TREE, build_int_cst (signed_char_type_node, cmode));

  ctor = finish_compound_literal (get_pseudo_contract_violation_type (),
				  ctor, tf_none);
  protected_set_expr_location (ctor, EXPR_LOCATION (contract));
  return ctor;
}

/* Return handle_contract_violation(), declaring it if needed.  */

static tree
declare_handle_contract_violation ()
{
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

  tree id_exp = get_identifier ("experimental");
  tree ns_exp = lookup_qualified_name (std_node, id_exp);

  tree violation = error_mark_node;
  if (TREE_CODE (ns_exp) == NAMESPACE_DECL)
    violation = lookup_qualified_name (ns_exp, viol_name,
				       LOOK_want::TYPE
				       |LOOK_want::HIDDEN_FRIEND);

  if (TREE_CODE (violation) == TYPE_DECL)
    violation = TREE_TYPE (violation);
  else
    {
      push_nested_namespace (std_node);
      push_namespace (id_exp, /*inline*/false);
      violation = make_class_type (RECORD_TYPE);
      create_implicit_typedef (viol_name, violation);
      DECL_SOURCE_LOCATION (TYPE_NAME (violation)) = BUILTINS_LOCATION;
      DECL_CONTEXT (TYPE_NAME (violation)) = current_namespace;
      pushdecl_namespace_level (TYPE_NAME (violation), /*hidden*/true);
      pop_namespace ();
      pop_nested_namespace (std_node);
    }

  tree argtype = cp_build_qualified_type (violation, TYPE_QUAL_CONST);
  argtype = cp_build_reference_type (argtype, /*rval*/false);
  tree fntype = build_function_type_list (void_type_node, argtype, NULL_TREE);

  push_nested_namespace (global_namespace);
  tree fn = build_cp_library_fn_ptr ("handle_contract_violation", fntype,
				     ECF_COLD);
  pushdecl_namespace_level (fn, /*hiding*/true);
  pop_nested_namespace (global_namespace);

  return fn;
}

/* Build the call to handle_contract_violation for CONTRACT.  */

static void
build_contract_handler_call (tree contract,
			     contract_continuation cmode)
{
  tree violation = build_contract_violation (contract, cmode);
  tree violation_fn = declare_handle_contract_violation ();
  tree call = build_call_n (violation_fn, 1, build_address (violation));
  finish_expr_stmt (call);
}

/* Generate the code that checks or assumes a contract, but do not attach
   it to the current context.  This is called during genericization.  */

tree
build_contract_check (tree contract)
{
  contract_semantic semantic = get_contract_semantic (contract);
  if (semantic == CCS_INVALID)
    return NULL_TREE;

  /* Ignored contracts are never checked or assumed.  */
  if (semantic == CCS_IGNORE)
    return void_node;

  remap_dummy_this (current_function_decl, &CONTRACT_CONDITION (contract));
  tree condition = CONTRACT_CONDITION (contract);
  if (condition == error_mark_node)
    return NULL_TREE;

  location_t loc = EXPR_LOCATION (contract);

  if (semantic == CCS_ASSUME)
    return build_assume_call (loc, condition);

  tree if_stmt = begin_if_stmt ();
  tree cond = build_x_unary_op (loc,
				TRUTH_NOT_EXPR,
				condition, NULL_TREE,
				tf_warning_or_error);
  finish_if_stmt_cond (cond, if_stmt);

  /* Get the continuation mode.  */
  contract_continuation cmode;
  switch (semantic)
    {
    case CCS_NEVER: cmode = NEVER_CONTINUE; break;
    case CCS_MAYBE: cmode = MAYBE_CONTINUE; break;
    default: gcc_unreachable ();
    }

  build_contract_handler_call (contract, cmode);
  if (cmode == NEVER_CONTINUE)
    finish_expr_stmt (build_call_a (terminate_fn, 0, nullptr));

  finish_then_clause (if_stmt);
  tree scope = IF_SCOPE (if_stmt);
  IF_SCOPE (if_stmt) = NULL;
  return do_poplevel (scope);
}

/* Add the contract statement CONTRACT to the current block if valid.  */

static void
emit_contract_statement (tree contract)
{
  /* Only add valid contracts.  */
  if (get_contract_semantic (contract) != CCS_INVALID
      && CONTRACT_CONDITION (contract) != error_mark_node)
    add_stmt (contract);
}

/* Generate the statement for the given contract attribute by adding the
   statement to the current block. Returns the next contract in the chain.  */

static tree
emit_contract_attr (tree attr)
{
  gcc_assert (TREE_CODE (attr) == TREE_LIST);

  emit_contract_statement (CONTRACT_STATEMENT (attr));

  return CONTRACT_CHAIN (attr);
}

/* Add the statements of contract attributes ATTRS to the current block.  */

static void
emit_contract_conditions (tree attrs, tree_code code)
{
  if (!attrs) return;
  gcc_assert (TREE_CODE (attrs) == TREE_LIST);
  gcc_assert (code == PRECONDITION_STMT || code == POSTCONDITION_STMT);
  while (attrs)
    {
      tree contract = CONTRACT_STATEMENT (attrs);
      if (TREE_CODE (contract) == code)
	attrs = emit_contract_attr (attrs);
      else
	attrs = CONTRACT_CHAIN (attrs);
    }
}

/* Emit the statement for an assertion attribute.  */

void
emit_assertion (tree attr)
{
  emit_contract_attr (attr);
}

/* Emit statements for precondition attributes.  */

static void
emit_preconditions (tree attr)
{
  return emit_contract_conditions (attr, PRECONDITION_STMT);
}

/* Emit statements for postcondition attributes.  */

static void
emit_postconditions (tree attr)
{
  return emit_contract_conditions (attr, POSTCONDITION_STMT);
}

/* We're compiling the pre/postcondition function CONDFN; remap any FN
   attributes that match CODE and emit them.  */

static void
remap_and_emit_conditions (tree fn, tree condfn, tree_code code)
{
  gcc_assert (code == PRECONDITION_STMT || code == POSTCONDITION_STMT);
  for (tree attr = DECL_CONTRACTS (fn); attr;
       attr = CONTRACT_CHAIN (attr))
    {
      tree contract = CONTRACT_STATEMENT (attr);
      if (TREE_CODE (contract) == code)
	{
	  contract = copy_node (contract);
	  remap_contract (fn, condfn, contract, /*duplicate_p=*/false);
	  emit_contract_statement (contract);
	}
    }
}

/* Converts a contract condition to bool and ensures it has a locaiton.  */

tree
finish_contract_condition (cp_expr condition)
{
  /* Ensure we have the condition location saved in case we later need to
     emit a conversion error during template instantiation and wouldn't
     otherwise have it.  */
  if (!CAN_HAVE_LOCATION_P (condition) || EXCEPTIONAL_CLASS_P (condition))
    {
      condition = build1_loc (condition.get_location (), VIEW_CONVERT_EXPR,
			      TREE_TYPE (condition), condition);
      EXPR_LOCATION_WRAPPER_P (condition) = 1;
    }

  if (condition == error_mark_node || type_dependent_expression_p (condition))
    return condition;

  return condition_conversion (condition);
}

void
maybe_update_postconditions (tree fco)
{
  /* Update any postconditions and the postcondition checking function
     as needed.  If there are postconditions, we'll use those to rewrite
     return statements to check postconditions.  */
  if (has_active_postconditions (fco))
    {
      rebuild_postconditions (fco);
      tree post = build_postcondition_function (fco);
      set_postcondition_function (fco, post);
    }
}

/* Called on attribute lists that must not contain contracts.  If any
   contracts are present, issue an error diagnostic and return true.  */

bool
diagnose_misapplied_contracts (tree attributes)
{
  if (attributes == NULL_TREE)
    return false;

  tree contract_attr = find_contract (attributes);
  if (!contract_attr)
    return false;

  error_at (EXPR_LOCATION (CONTRACT_STATEMENT (contract_attr)),
	    "contracts must appertain to a function type");

  /* Invalidate the contract so we don't treat it as valid later on.  */
  invalidate_contract (TREE_VALUE (TREE_VALUE (contract_attr)));

  return true;
}

/* Build and return an argument list containing all the parameters of the
   (presumably guarded) FUNCTION_DECL FN.  This can be used to forward all of
   FN's arguments to a function taking the same list of arguments -- namely
   the unchecked form of FN.

   We use CALL_FROM_THUNK_P instead of forward_parm for forwarding
   semantics.  */

static vec<tree, va_gc> *
build_arg_list (tree fn)
{
  vec<tree, va_gc> *args = make_tree_vector ();
  for (tree t = DECL_ARGUMENTS (fn); t; t = DECL_CHAIN (t))
    vec_safe_push (args, t);
  return args;
}

void
start_function_contracts (tree decl1)
{
  if (!handle_contracts_p (decl1))
    return;

  /* For cdtors, we evaluate the contracts check inline.  */
  if (!outline_contracts_p (decl1))
    return;

  /* Contracts may have just been added without a chance to parse them, though
     we still need the PRE_FN available to generate a call to it.  */
  if (!DECL_PRE_FN (decl1))
    build_contract_function_decls (decl1);

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
  tree call = build_call_a (DECL_PRE_FN (fndecl), args->length (),
			    args->address ());
  CALL_FROM_THUNK_P (call) = true;
  finish_expr_stmt (call);
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
  tree call = build_call_a (DECL_POST_FN (fndecl), args->length (),
			    args->address ());
  CALL_FROM_THUNK_P (call) = true;
  finish_expr_stmt (call);
}

/* Add a call or a direct evaluation of the pre checks.  */

static void
apply_preconditions (tree fndecl)
{
  if (outline_contracts_p (fndecl))
    add_pre_condition_fn_call (fndecl);
  else
    emit_preconditions (DECL_CONTRACTS (fndecl));
}

/* Add a call or a direct evaluation of the post checks.  */

static void
apply_postconditions (tree fndecl)
{
  if (outline_contracts_p (fndecl))
    add_post_condition_fn_call (fndecl);
  else
    emit_postconditions (DECL_CONTRACTS (fndecl));
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

  bool do_pre = has_active_preconditions (fndecl);
  bool do_post = has_active_postconditions (fndecl);
  /* We should not have reached here with nothing to do... */
  gcc_checking_assert (do_pre || do_post);

  /* This copies the approach used for function try blocks.  */
  tree fnbody = pop_stmt_list (DECL_SAVED_TREE (fndecl));
  DECL_SAVED_TREE (fndecl) = push_stmt_list ();
  tree compound_stmt = begin_compound_stmt (0);
  current_binding_level->artificial = 1;

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
      TREE_OPERAND (eh_else, 1) = build_empty_stmt (loc);
    }
  else
    apply_postconditions (fndecl);
  TREE_OPERAND (try_fin, 1) = pop_stmt_list (TREE_OPERAND (try_fin, 1));
  finish_compound_stmt (compound_stmt);
  /* The DECL_SAVED_TREE stmt list will be popped by our caller.  */
}

/* Finish up the pre & post function definitions for a guarded FNDECL,
   and compile those functions all the way to assembler language output.  */

void
finish_function_contracts (tree fndecl)
{
  if (!handle_contracts_p (fndecl)
      || !outline_contracts_p (fndecl))
    return;

  for (tree ca = DECL_CONTRACTS (fndecl); ca; ca = CONTRACT_CHAIN (ca))
    {
      tree contract = CONTRACT_STATEMENT (ca);
      if (!CONTRACT_CONDITION (contract)
	  || CONTRACT_CONDITION_DEFERRED_P (contract)
	  || CONTRACT_CONDITION (contract) == error_mark_node)
	return;
    }

  int flags = SF_DEFAULT | SF_PRE_PARSED;

  /* If either the pre or post functions are bad, don't bother emitting
     any contracts.  The program is already ill-formed.  */
  tree pre = DECL_PRE_FN (fndecl);
  tree post = DECL_POST_FN (fndecl);
  if (pre == error_mark_node || post == error_mark_node)
    return;

  if (pre && DECL_INITIAL (fndecl) != error_mark_node)
    {
      DECL_PENDING_INLINE_P (pre) = false;
      start_preparsed_function (pre, DECL_ATTRIBUTES (pre), flags);
      remap_and_emit_conditions (fndecl, pre, PRECONDITION_STMT);
      tree finished_pre = finish_function (false);
      expand_or_defer_fn (finished_pre);
    }

  if (post && DECL_INITIAL (fndecl) != error_mark_node)
    {
      DECL_PENDING_INLINE_P (post) = false;
      start_preparsed_function (post,
				DECL_ATTRIBUTES (post),
				flags);
      remap_and_emit_conditions (fndecl, post, POSTCONDITION_STMT);
      if (!VOID_TYPE_P (TREE_TYPE (TREE_TYPE (post))))
	finish_return_stmt (get_postcondition_result_parameter (fndecl));

      tree finished_post = finish_function (false);
      expand_or_defer_fn (finished_post);
    }
}


/* A subroutine of duplicate_decls. Diagnose issues in the redeclaration of
   guarded functions.  */

void
duplicate_contracts (tree newdecl, tree olddecl)
{
  if (TREE_CODE (newdecl) == TEMPLATE_DECL)
    newdecl = DECL_TEMPLATE_RESULT (newdecl);
  if (TREE_CODE (olddecl) == TEMPLATE_DECL)
    olddecl = DECL_TEMPLATE_RESULT (olddecl);

  /* Compare contracts to see if they match.    */
  tree old_contracts = DECL_CONTRACTS (olddecl);
  tree new_contracts = DECL_CONTRACTS (newdecl);

  if (!old_contracts && !new_contracts)
    return;

  location_t old_loc = DECL_SOURCE_LOCATION (olddecl);
  location_t new_loc = DECL_SOURCE_LOCATION (newdecl);

  /* If both declarations specify contracts, ensure they match.

     TODO: This handles a potential error a little oddly. Consider:

	struct B {
	  virtual void f(int n) [[pre: n == 0]];
	};
	struct D : B {
	  void f(int n) override; // inherits contracts
	};
	void D::f(int n) [[pre: n == 0]] // OK
	{ }

    It's okay because we're explicitly restating the inherited contract.
    Changing the precondition on the definition D::f causes match_contracts
    to complain about the mismatch.

    This would previously have been diagnosed as adding contracts to an
    override, but this seems like it should be well-formed.  */
  if (old_contracts && new_contracts)
    {
      if (!match_contract_conditions (old_loc, old_contracts,
				      new_loc, new_contracts,
				      cmc_declaration))
	return;
      if (DECL_UNIQUE_FRIEND_P (newdecl))
	/* Newdecl's contracts are still DEFERRED_PARSE, and we're about to
	   collapse it into olddecl, so stash away olddecl's contracts for
	   later comparison.  */
	defer_guarded_contract_match (olddecl, olddecl, old_contracts);
    }

  /* Handle cases where contracts are omitted in one or the other
     declaration.  */
  if (old_contracts)
    {
      /* Contracts have been previously specified by are no omitted. The
	 new declaration inherits the existing contracts. */
      if (!new_contracts)
	copy_contract_attributes (newdecl, olddecl);

      /* In all cases, remove existing contracts from OLDDECL to prevent the
	 attribute merging function from adding excess contracts.  */
      remove_contract_attributes (olddecl);
    }
  else if (!old_contracts)
    {
      /* We are adding contracts to a declaration.  */
      if (new_contracts)
	{
	  /* We can't add to a previously defined function.  */
	  if (DECL_INITIAL (olddecl))
	    {
	      auto_diagnostic_group d;
	      error_at (new_loc, "cannot add contracts after definition");
	      inform (DECL_SOURCE_LOCATION (olddecl), "original definition here");
	      return;
	    }

	  /* We can't add to an unguarded virtual function declaration.  */
	  if (DECL_VIRTUAL_P (olddecl) && new_contracts)
	    {
	      auto_diagnostic_group d;
	      error_at (new_loc, "cannot add contracts to a virtual function");
	      inform (DECL_SOURCE_LOCATION (olddecl), "original declaration here");
	      return;
	    }

	  /* Depending on the "first declaration" rule, we may not be able
	     to add contracts to a function after the fact.  */
	  if (flag_contract_strict_declarations)
	    {
	      warning_at (new_loc,
			  OPT_fcontract_strict_declarations_,
			  "declaration adds contracts to %q#D",
			  olddecl);
	      return;
	    }

	  /* Copy the contracts from NEWDECL to OLDDECL. We shouldn't need to
	     remap them because NEWDECL's parameters will replace those of
	     OLDDECL.  Remove the contracts from NEWDECL so they aren't
	     cloned when merging.  */
	  copy_contract_attributes (olddecl, newdecl);
	  remove_contract_attributes (newdecl);
	}
    }
}

/* Replace the any contract attributes on OVERRIDER with a copy where any
   references to BASEFN's PARM_DECLs have been rewritten to the corresponding
   PARM_DECL in OVERRIDER.  */

void
inherit_base_contracts (tree overrider, tree basefn)
{
  tree last = NULL_TREE, contract_attrs = NULL_TREE;
  for (tree a = DECL_CONTRACTS (basefn);
      a != NULL_TREE;
      a = CONTRACT_CHAIN (a))
    {
      tree c = copy_node (a);
      TREE_VALUE (c) = build_tree_list (TREE_PURPOSE (TREE_VALUE (c)),
					copy_node (CONTRACT_STATEMENT (c)));

      tree src = basefn;
      tree dst = overrider;
      remap_contract (src, dst, CONTRACT_STATEMENT (c), /*duplicate_p=*/true);

      CONTRACT_COMMENT (CONTRACT_STATEMENT (c)) =
	copy_node (CONTRACT_COMMENT (CONTRACT_STATEMENT (c)));

      chainon (last, c);
      last = c;
      if (!contract_attrs)
	contract_attrs = c;
    }

  set_decl_contracts (overrider, contract_attrs);
}

#include "gt-cp-contracts.h"
