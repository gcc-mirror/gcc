/* Definitions for C++ contract levels.  Implements functionality described in
   the N4820 working draft version of contracts, P1290, P1332, and P1429.
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

#ifndef GCC_CP_CONTRACT_H
#define GCC_CP_CONTRACT_H

/* Contract levels approximate the complexity of the expression.  */

enum contract_level
{
  CONTRACT_INVALID,
  CONTRACT_DEFAULT,
  CONTRACT_AUDIT,
  CONTRACT_AXIOM
};

/* The concrete semantics determine the behavior of a contract.  */

enum contract_semantic
{
  CCS_INVALID,
  CCS_IGNORE,
  CCS_ASSUME,
  CCS_NEVER,
  CCS_MAYBE
};

/* True if the contract is unchecked.  */

inline bool
unchecked_contract_p (contract_semantic cs)
{
  return cs == CCS_IGNORE || cs == CCS_ASSUME;
}

/* True if the contract is checked.  */

inline bool
checked_contract_p (contract_semantic cs)
{
  return cs >= CCS_NEVER;
}

/* Must match std::contract_violation_continuation_mode in <contract>.  */
enum contract_continuation
{
  NEVER_CONTINUE,
  MAYBE_CONTINUE
};

/* Assertion role info.  */
struct contract_role
{
  const char *name;
  contract_semantic default_semantic;
  contract_semantic audit_semantic;
  contract_semantic axiom_semantic;
};

/* Information for configured contract semantics.  */

struct contract_configuration
{
  contract_level level;
  contract_role* role;
};

/* A contract mode contains information used to derive the checking
   and assumption semantics of a contract. This is either a dynamic
   configuration, meaning it derives from the build mode, or it is
   explicitly specified.  */

struct contract_mode
{
  contract_mode () : kind(cm_invalid) {}
  contract_mode (contract_level level, contract_role *role = NULL)
    : kind(cm_dynamic)
  {
    contract_configuration cc;
    cc.level = level;
    cc.role = role;
    u.config = cc;
  }
  contract_mode (contract_semantic semantic) : kind(cm_explicit)
  {
    u.semantic = semantic;
  }

  contract_level get_level () const
  {
    gcc_assert (kind == cm_dynamic);
    return u.config.level;
  }

  contract_role *get_role () const
  {
    gcc_assert (kind == cm_dynamic);
    return u.config.role;
  }

  contract_semantic get_semantic () const
  {
    gcc_assert (kind == cm_explicit);
    return u.semantic;
  }

  enum { cm_invalid, cm_dynamic, cm_explicit } kind;

  union
  {
    contract_configuration config;
    contract_semantic semantic;
  } u;
};

extern contract_role *get_contract_role	(const char *);
extern contract_role *add_contract_role	(const char *,
					 contract_semantic,
					 contract_semantic,
					 contract_semantic,
					 bool = true);
extern void validate_contract_role	(contract_role *);
extern void setup_default_contract_role	(bool = true);
extern contract_semantic lookup_concrete_semantic (const char *);

/* Map a source level semantic or level name to its value, or invalid.  */
extern contract_semantic map_contract_semantic	(const char *);
extern contract_level map_contract_level	(const char *);

/* Check if an attribute is a cxx contract attribute.  */
extern bool cxx_contract_attribute_p (const_tree);
extern bool cp_contract_assertion_p (const_tree);

/* Returns the default role.  */

inline contract_role *
get_default_contract_role ()
{
  return get_contract_role ("default");
}

/* Handle various command line arguments related to semantic mapping.  */
extern void handle_OPT_fcontract_build_level_ (const char *);
extern void handle_OPT_fcontract_assumption_mode_ (const char *);
extern void handle_OPT_fcontract_continuation_mode_ (const char *);
extern void handle_OPT_fcontract_role_ (const char *);
extern void handle_OPT_fcontract_semantic_ (const char *);

enum contract_matching_context
{
  cmc_declaration,
  cmc_override
};

/* True if NODE is any kind of contract.  */
#define CONTRACT_P(NODE)			\
  (TREE_CODE (NODE) == ASSERTION_STMT		\
   || TREE_CODE (NODE) == PRECONDITION_STMT	\
   || TREE_CODE (NODE) == POSTCONDITION_STMT)

/* True if NODE is a contract condition.  */
#define CONTRACT_CONDITION_P(NODE)		\
  (TREE_CODE (NODE) == PRECONDITION_STMT	\
   || TREE_CODE (NODE) == POSTCONDITION_STMT)

/* True if NODE is a precondition.  */
#define PRECONDITION_P(NODE)           \
  (TREE_CODE (NODE) == PRECONDITION_STMT)

/* True if NODE is a postcondition.  */
#define POSTCONDITION_P(NODE)          \
  (TREE_CODE (NODE) == POSTCONDITION_STMT)

#define CONTRACT_CHECK(NODE) \
  (TREE_CHECK3 (NODE, ASSERTION_STMT, PRECONDITION_STMT, POSTCONDITION_STMT))

/* True iff the FUNCTION_DECL NODE currently has any contracts.  */
#define DECL_HAS_CONTRACTS_P(NODE) \
  (DECL_CONTRACTS (NODE) != NULL_TREE)

/* For a FUNCTION_DECL of a guarded function, this points to a list of the pre
   and post contracts of the first decl of NODE in original order. */
#define DECL_CONTRACTS(NODE) \
  (find_contract (DECL_ATTRIBUTES (NODE)))

/* The next contract (if any) after this one in an attribute list.  */
#define CONTRACT_CHAIN(NODE) \
  (find_contract (TREE_CHAIN (NODE)))

/* The wrapper of the original source location of a list of contracts.  */
#define CONTRACT_SOURCE_LOCATION_WRAPPER(NODE) \
  (TREE_PURPOSE (TREE_VALUE (NODE)))

/* The original source location of a list of contracts.  */
#define CONTRACT_SOURCE_LOCATION(NODE) \
  (EXPR_LOCATION (CONTRACT_SOURCE_LOCATION_WRAPPER (NODE)))

/* The actual code _STMT for a contract attribute.  */
#define CONTRACT_STATEMENT(NODE) \
  (TREE_VALUE (TREE_VALUE (NODE)))

/* True if the contract semantic was specified literally. If true, the
   contract mode is an identifier containing the semantic. Otherwise,
   it is a TREE_LIST whose TREE_VALUE is the level and whose TREE_PURPOSE
   is the role.  */
#define CONTRACT_LITERAL_MODE_P(NODE) \
  (CONTRACT_MODE (NODE) != NULL_TREE \
   && TREE_CODE (CONTRACT_MODE (NODE)) == IDENTIFIER_NODE)

/* The identifier denoting the literal semantic of the contract.  */
#define CONTRACT_LITERAL_SEMANTIC(NODE) \
  (TREE_OPERAND (NODE, 0))

/* The written "mode" of the contract. Either an IDENTIFIER with the
   literal semantic or a TREE_LIST containing the level and role.  */
#define CONTRACT_MODE(NODE) \
  (TREE_OPERAND (CONTRACT_CHECK (NODE), 0))

/* The identifier denoting the build level of the contract. */
#define CONTRACT_LEVEL(NODE)		\
  (TREE_VALUE (CONTRACT_MODE (NODE)))

/* The identifier denoting the role of the contract */
#define CONTRACT_ROLE(NODE)		\
  (TREE_PURPOSE (CONTRACT_MODE (NODE)))

/* The parsed condition of the contract.  */
#define CONTRACT_CONDITION(NODE) \
  (TREE_OPERAND (CONTRACT_CHECK (NODE), 1))

/* True iff the condition of the contract NODE is not yet parsed.  */
#define CONTRACT_CONDITION_DEFERRED_P(NODE) \
  (TREE_CODE (CONTRACT_CONDITION (NODE)) == DEFERRED_PARSE)

/* The raw comment of the contract.  */
#define CONTRACT_COMMENT(NODE) \
  (TREE_OPERAND (CONTRACT_CHECK (NODE), 2))

/* The VAR_DECL of a postcondition result. For deferred contracts, this
   is an IDENTIFIER.  */
#define POSTCONDITION_IDENTIFIER(NODE) \
  (TREE_OPERAND (POSTCONDITION_STMT_CHECK (NODE), 3))

/* For a FUNCTION_DECL of a guarded function, this holds the function decl
   where pre contract checks are emitted.  */
#define DECL_PRE_FN(NODE) \
  (get_precondition_function ((NODE)))

/* For a FUNCTION_DECL of a guarded function, this holds the function decl
   where post contract checks are emitted.  */
#define DECL_POST_FN(NODE) \
  (get_postcondition_function ((NODE)))

/* True iff the FUNCTION_DECL is the pre function for a guarded function.  */
#define DECL_IS_PRE_FN_P(NODE) \
  (DECL_ABSTRACT_ORIGIN (NODE) && DECL_PRE_FN (DECL_ABSTRACT_ORIGIN (NODE)) == NODE)

/* True iff the FUNCTION_DECL is the post function for a guarded function.  */
#define DECL_IS_POST_FN_P(NODE) \
  (DECL_ABSTRACT_ORIGIN (NODE) && DECL_POST_FN (DECL_ABSTRACT_ORIGIN (NODE)) == NODE)

extern void remove_contract_attributes		(tree);
extern void copy_contract_attributes		(tree, tree);
extern void remap_contracts			(tree, tree, tree, bool);
extern void maybe_update_postconditions		(tree);
extern void rebuild_postconditions		(tree);
extern bool check_postcondition_result		(tree, tree, location_t);
extern tree get_precondition_function		(tree);
extern tree get_postcondition_function		(tree);
extern void duplicate_contracts			(tree, tree);
extern void match_deferred_contracts		(tree);
extern void defer_guarded_contract_match	(tree, tree, tree);
extern bool diagnose_misapplied_contracts	(tree);
extern tree finish_contract_attribute		(tree, tree);
extern tree invalidate_contract			(tree);
extern void update_late_contract		(tree, tree, tree);
extern tree splice_out_contracts		(tree);
extern bool all_attributes_are_contracts_p	(tree);
extern void inherit_base_contracts		(tree, tree);
extern void start_function_contracts		(tree);
extern void maybe_apply_function_contracts	(tree);
extern void finish_function_contracts		(tree);
extern void set_contract_functions		(tree, tree, tree);
extern tree build_contract_check		(tree);
extern void emit_assertion			(tree);

#endif /* ! GCC_CP_CONTRACT_H */
