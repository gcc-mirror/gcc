/* Data structures and function exported by the C++ Parser.
   Copyright (C) 2010-2016 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_CP_PARSER_H
#define GCC_CP_PARSER_H

#include "tree.h"
#include "cp/cp-tree.h"
#include "c-family/c-pragma.h"

/* A token's value and its associated deferred access checks and
   qualifying scope.  */

struct GTY(()) tree_check {
  /* The value associated with the token.  */
  tree value;
  /* The checks that have been associated with value.  */
  vec<deferred_access_check, va_gc> *checks;
  /* The token's qualifying scope (used when it is a
     CPP_NESTED_NAME_SPECIFIER).  */
  tree qualifying_scope;
};

/* A C++ token.  */

struct GTY (()) cp_token {
  /* The kind of token.  */
  ENUM_BITFIELD (cpp_ttype) type : 8;
  /* If this token is a keyword, this value indicates which keyword.
     Otherwise, this value is RID_MAX.  */
  ENUM_BITFIELD (rid) keyword : 8;
  /* Token flags.  */
  unsigned char flags;
  /* True if this token is from a context where it is implicitly extern "C" */
  BOOL_BITFIELD implicit_extern_c : 1;
  /* True if an error has already been reported for this token, such as a
     CPP_NAME token that is not a keyword (i.e., for which KEYWORD is
     RID_MAX) iff this name was looked up and found to be ambiguous.  */
  BOOL_BITFIELD error_reported : 1;
  /* True for a token that has been purged.  If a token is purged,
     it is no longer a valid token and it should be considered
     deleted.  */
  BOOL_BITFIELD purged_p : 1;
  /* 5 unused bits.  */
  /* The location at which this token was found.  */
  location_t location;
  /* The value associated with this token, if any.  */
  union cp_token_value {
    /* Used for compound tokens such as CPP_NESTED_NAME_SPECIFIER.  */
    struct tree_check* GTY((tag ("1"))) tree_check_value;
    /* Use for all other tokens.  */
    tree GTY((tag ("0"))) value;
  } GTY((desc ("(%1.type == CPP_TEMPLATE_ID)"
	       "|| (%1.type == CPP_NESTED_NAME_SPECIFIER)"
	       "|| (%1.type == CPP_DECLTYPE)"))) u;
};


/* We use a stack of token pointer for saving token sets.  */
typedef struct cp_token *cp_token_position;

/* The cp_lexer structure represents the C++ lexer.  It is responsible
   for managing the token stream from the preprocessor and supplying
   it to the parser.  Tokens are never added to the cp_lexer after
   it is created.  */

struct GTY (()) cp_lexer {
  /* The memory allocated for the buffer.  NULL if this lexer does not
     own the token buffer.  */
  vec<cp_token, va_gc> *buffer;

  /* A pointer just past the last available token.  The tokens
     in this lexer are [buffer, last_token).  */
  cp_token_position GTY ((skip)) last_token;

  /* The next available token.  If NEXT_TOKEN is &eof_token, then there are
     no more available tokens.  */
  cp_token_position GTY ((skip)) next_token;

  /* A stack indicating positions at which cp_lexer_save_tokens was
     called.  The top entry is the most recent position at which we
     began saving tokens.  If the stack is non-empty, we are saving
     tokens.  */
  vec<cp_token_position> GTY ((skip)) saved_tokens;

  /* The next lexer in a linked list of lexers.  */
  struct cp_lexer *next;

  /* True if we should output debugging information.  */
  bool debugging_p;

  /* True if we're in the context of parsing a pragma, and should not
     increment past the end-of-line marker.  */
  bool in_pragma;
};


/* cp_token_cache is a range of tokens.  There is no need to represent
   allocate heap memory for it, since tokens are never removed from the
   lexer's array.  There is also no need for the GC to walk through
   a cp_token_cache, since everything in here is referenced through
   a lexer.  */

struct GTY(()) cp_token_cache {
  /* The beginning of the token range.  */
  cp_token * GTY((skip)) first;

  /* Points immediately after the last token in the range.  */
  cp_token * GTY ((skip)) last;
};

typedef cp_token_cache *cp_token_cache_ptr;

struct cp_token_ident
{
  unsigned int ident_len;
  const char *ident_str;
  unsigned int before_len;
  const char *before_str;
  unsigned int after_len;
  const char *after_str;
};

/* An entry in a queue of function arguments that require post-processing.  */

struct GTY(()) cp_default_arg_entry {
  /* The current_class_type when we parsed this arg.  */
  tree class_type;

  /* The function decl itself.  */
  tree decl;
};


/* An entry in a stack for member functions defined within their classes.  */

struct GTY(()) cp_unparsed_functions_entry {
  /* Functions with default arguments that require post-processing.
     Functions appear in this list in declaration order.  */
  vec<cp_default_arg_entry, va_gc> *funs_with_default_args;

  /* Functions with defintions that require post-processing.  Functions
     appear in this list in declaration order.  */
  vec<tree, va_gc> *funs_with_definitions;

  /* Non-static data members with initializers that require post-processing.
     FIELD_DECLs appear in this list in declaration order.  */
  vec<tree, va_gc> *nsdmis;

  /* Nested classes go in this vector, so that we can do some final
     processing after parsing any NSDMIs.  */
  vec<tree, va_gc> *classes;
};


/* The status of a tentative parse.  */

enum cp_parser_status_kind
{
  /* No errors have occurred.  */
  CP_PARSER_STATUS_KIND_NO_ERROR,
  /* An error has occurred.  */
  CP_PARSER_STATUS_KIND_ERROR,
  /* We are committed to this tentative parse, whether or not an error
     has occurred.  */
  CP_PARSER_STATUS_KIND_COMMITTED
};


/* Context that is saved and restored when parsing tentatively.  */
struct GTY (()) cp_parser_context {
  /* If this is a tentative parsing context, the status of the
     tentative parse.  */
  enum cp_parser_status_kind status;
  /* If non-NULL, we have just seen a `x->' or `x.' expression.  Names
     that are looked up in this context must be looked up both in the
     scope given by OBJECT_TYPE (the type of `x' or `*x') and also in
     the context of the containing expression.  */
  tree object_type;

  /* The next parsing context in the stack.  */
  struct cp_parser_context *next;
};


/* Helper data structure for parsing #pragma omp declare simd, and Cilk Plus
   SIMD-enabled functions' vector attribute.  */
struct cp_omp_declare_simd_data {
  bool error_seen; /* Set if error has been reported.  */
  bool fndecl_seen; /* Set if one fn decl/definition has been seen already.  */
  vec<cp_token_cache_ptr> tokens;
  tree clauses;
};

/* Helper data structure for parsing #pragma acc routine.  */
struct cp_oacc_routine_data : cp_omp_declare_simd_data {
  location_t loc;
};

/* The cp_parser structure represents the C++ parser.  */

struct GTY(()) cp_parser {
  /* The lexer from which we are obtaining tokens.  */
  cp_lexer *lexer;

  /* The scope in which names should be looked up.  If NULL_TREE, then
     we look up names in the scope that is currently open in the
     source program.  If non-NULL, this is either a TYPE or
     NAMESPACE_DECL for the scope in which we should look.  It can
     also be ERROR_MARK, when we've parsed a bogus scope.

     This value is not cleared automatically after a name is looked
     up, so we must be careful to clear it before starting a new look
     up sequence.  (If it is not cleared, then `X::Y' followed by `Z'
     will look up `Z' in the scope of `X', rather than the current
     scope.)  Unfortunately, it is difficult to tell when name lookup
     is complete, because we sometimes peek at a token, look it up,
     and then decide not to consume it.   */
  tree scope;

  /* OBJECT_SCOPE and QUALIFYING_SCOPE give the scopes in which the
     last lookup took place.  OBJECT_SCOPE is used if an expression
     like "x->y" or "x.y" was used; it gives the type of "*x" or "x",
     respectively.  QUALIFYING_SCOPE is used for an expression of the
     form "X::Y"; it refers to X.  */
  tree object_scope;
  tree qualifying_scope;

  /* A stack of parsing contexts.  All but the bottom entry on the
     stack will be tentative contexts.

     We parse tentatively in order to determine which construct is in
     use in some situations.  For example, in order to determine
     whether a statement is an expression-statement or a
     declaration-statement we parse it tentatively as a
     declaration-statement.  If that fails, we then reparse the same
     token stream as an expression-statement.  */
  cp_parser_context *context;

  /* True if we are parsing GNU C++.  If this flag is not set, then
     GNU extensions are not recognized.  */
  bool allow_gnu_extensions_p;

  /* TRUE if the `>' token should be interpreted as the greater-than
     operator.  FALSE if it is the end of a template-id or
     template-parameter-list. In C++0x mode, this flag also applies to
     `>>' tokens, which are viewed as two consecutive `>' tokens when
     this flag is FALSE.  */
  bool greater_than_is_operator_p;

  /* TRUE if default arguments are allowed within a parameter list
     that starts at this point. FALSE if only a gnu extension makes
     them permissible.  */
  bool default_arg_ok_p;

  /* TRUE if we are parsing an integral constant-expression.  See
     [expr.const] for a precise definition.  */
  bool integral_constant_expression_p;

  /* TRUE if we are parsing an integral constant-expression -- but a
     non-constant expression should be permitted as well.  This flag
     is used when parsing an array bound so that GNU variable-length
     arrays are tolerated.  */
  bool allow_non_integral_constant_expression_p;

  /* TRUE if ALLOW_NON_CONSTANT_EXPRESSION_P is TRUE and something has
     been seen that makes the expression non-constant.  */
  bool non_integral_constant_expression_p;

  /* TRUE if local variable names and `this' are forbidden in the
     current context.  */
  bool local_variables_forbidden_p;

  /* TRUE if the declaration we are parsing is part of a
     linkage-specification of the form `extern string-literal
     declaration'.  */
  bool in_unbraced_linkage_specification_p;

  /* TRUE if we are presently parsing a declarator, after the
     direct-declarator.  */
  bool in_declarator_p;

  /* TRUE if we are presently parsing a template-argument-list.  */
  bool in_template_argument_list_p;

  /* Set to IN_ITERATION_STMT if parsing an iteration-statement,
     to IN_OMP_BLOCK if parsing OpenMP structured block and
     IN_OMP_FOR if parsing OpenMP loop.  If parsing a switch statement,
     this is bitwise ORed with IN_SWITCH_STMT, unless parsing an
     iteration-statement, OpenMP block or loop within that switch.  */
#define IN_SWITCH_STMT		1
#define IN_ITERATION_STMT	2
#define IN_OMP_BLOCK		4
#define IN_OMP_FOR		8
#define IN_IF_STMT             16
#define IN_CILK_SIMD_FOR       32
#define IN_CILK_SPAWN          64
  unsigned char in_statement;

  /* TRUE if we are presently parsing the body of a switch statement.
     Note that this doesn't quite overlap with in_statement above.
     The difference relates to giving the right sets of error messages:
     "case not in switch" vs "break statement used with OpenMP...".  */
  bool in_switch_statement_p;

  /* TRUE if we are parsing a type-id in an expression context.  In
     such a situation, both "type (expr)" and "type (type)" are valid
     alternatives.  */
  bool in_type_id_in_expr_p;

  /* TRUE if we are currently in a header file where declarations are
     implicitly extern "C".  */
  bool implicit_extern_c;

  /* TRUE if strings in expressions should be translated to the execution
     character set.  */
  bool translate_strings_p;

  /* TRUE if we are presently parsing the body of a function, but not
     a local class.  */
  bool in_function_body;

  /* Nonzero if we're processing a __transaction_atomic or
     __transaction_relaxed statement.  */
  unsigned char in_transaction;

  /* TRUE if we can auto-correct a colon to a scope operator.  */
  bool colon_corrects_to_scope_p;

  /* TRUE if : doesn't start a class definition.  Should be only used
     together with type_definition_forbidden_message non-NULL, in
     contexts where new types may not be defined, and the type list
     is terminated by colon.  */
  bool colon_doesnt_start_class_def_p;

  /* If non-NULL, then we are parsing a construct where new type
     definitions are not permitted.  The string stored here will be
     issued as an error message if a type is defined.  */
  const char *type_definition_forbidden_message;

  /* A stack used for member functions of local classes.  The lists
     contained in an individual entry can only be processed once the
     outermost class being defined is complete.  */
  vec<cp_unparsed_functions_entry, va_gc> *unparsed_queues;

  /* The number of classes whose definitions are currently in
     progress.  */
  unsigned num_classes_being_defined;

  /* The number of template parameter lists that apply directly to the
     current declaration.  */
  unsigned num_template_parameter_lists;

  /* When parsing #pragma omp declare simd, this is a pointer to a
     helper data structure.  */
  cp_omp_declare_simd_data * GTY((skip)) omp_declare_simd;

  /* When parsing Cilk Plus SIMD-enabled functions' vector attributes,
     this is a pointer to a helper data structure.  */
  cp_omp_declare_simd_data * GTY((skip)) cilk_simd_fn_info;

  /* When parsing #pragma acc routine, this is a pointer to a helper data
     structure.  */
  cp_oacc_routine_data * GTY((skip)) oacc_routine;
  
  /* Nonzero if parsing a parameter list where 'auto' should trigger an implicit
     template parameter.  */
  bool auto_is_implicit_function_template_parm_p;

  /* TRUE if the function being declared was made a template due to its
     parameter list containing generic type specifiers (`auto' or concept
     identifiers) rather than an explicit template parameter list.  */
  bool fully_implicit_function_template_p;

  /* Tracks the function's template parameter list when declaring a function
     using generic type parameters.  This is either a new chain in the case of a
     fully implicit function template or an extension of the function's existing
     template parameter list.  This is tracked to optimize calls subsequent
     calls to synthesize_implicit_template_parm during
     cp_parser_parameter_declaration.  */
  tree implicit_template_parms;

  /* The scope into which an implicit template parameter list has been
     introduced or an existing template parameter list is being extended with
     implicit template parameters.  In most cases this is the sk_function_parms
     scope containing the use of a generic type.  In the case of an out-of-line
     member definition using a generic type, it is the sk_class scope.  */
  cp_binding_level* implicit_template_scope;

  /* True if parsing a result type in a compound requirement. This permits
     constrained-type-specifiers inside what would normally be a trailing
     return type. */
  bool in_result_type_constraint_p;

  /* True if a constrained-type-specifier is not allowed in this
     context e.g., because they could never be deduced.  */
  int prevent_constrained_type_specifiers;

};

/* In parser.c  */
extern void debug (cp_token &ref);
extern void debug (cp_token *ptr);
extern void cp_lexer_debug_tokens (vec<cp_token, va_gc> *);
extern void debug (vec<cp_token, va_gc> &ref);
extern void debug (vec<cp_token, va_gc> *ptr);
extern void cp_debug_parser (FILE *, cp_parser *);
extern void debug (cp_parser &ref);
extern void debug (cp_parser *ptr);
extern bool cp_keyword_starts_decl_specifier_p (enum rid keyword);

#endif  /* GCC_CP_PARSER_H  */
