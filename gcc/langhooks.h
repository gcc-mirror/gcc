/* The lang_hooks data structure.
   Copyright 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_LANG_HOOKS_H
#define GCC_LANG_HOOKS_H

/* This file should be #include-d after tree.h.  */

struct diagnostic_context;

/* A print hook for print_tree ().  */
typedef void (*lang_print_tree_hook) (FILE *, tree, int indent);

/* The following hooks are documented in langhooks.c.  Must not be
   NULL.  */

struct lang_hooks_for_tree_inlining
{
  tree (*walk_subtrees) (tree *, int *,
			 tree (*) (tree *, int *, void *),
			 void *, void *);
  int (*cannot_inline_tree_fn) (tree *);
  int (*disregard_inline_limits) (tree);
  tree (*add_pending_fn_decls) (void *, tree);
  int (*tree_chain_matters_p) (tree);
  int (*auto_var_in_fn_p) (tree, tree);
  tree (*copy_res_decl_for_inlining) (tree, tree, tree,
				      void *, int *, tree);
  int (*anon_aggr_type_p) (tree);
  bool (*var_mod_type_p) (tree);
  int (*start_inlining) (tree);
  void (*end_inlining) (tree);
  tree (*convert_parm_for_inlining) (tree, tree, tree, int);
  int (*estimate_num_insns) (tree);
};

struct lang_hooks_for_callgraph
{
  /* The node passed is a language-specific tree node.  If its contents
     are relevant to use of other declarations, mark them.  */
  tree (*analyze_expr) (tree *, int *, tree);

  /* Produce RTL for function passed as argument.  */
  void (*expand_function) (tree);
};

/* Lang hooks for management of language-specific data or status
   when entering / leaving functions etc.  */
struct lang_hooks_for_functions
{
  /* Called when entering a function.  */
  void (*init) (struct function *);

  /* Called when leaving a function.  */
  void (*final) (struct function *);

  /* Called when entering a nested function.  */
  void (*enter_nested) (struct function *);

  /* Called when leaving a nested function.  */
  void (*leave_nested) (struct function *);
};

/* Lang hooks for rtl code generation.  */
struct lang_hooks_for_rtl_expansion
{
  /* Called after expand_function_start, but before expanding the body.  */
  void (*start) (void);

  /* Called to expand each statement.  */
  void (*stmt) (tree);

  /* Called after expanding the body but before expand_function_end.  */
  void (*end) (void);
};

/* The following hooks are used by tree-dump.c.  */

struct lang_hooks_for_tree_dump
{
  /* Dump language-specific parts of tree nodes.  Returns nonzero if it
     does not want the usual dumping of the second argument.  */
  bool (*dump_tree) (void *, tree);

  /* Determine type qualifiers in a language-specific way.  */
  int (*type_quals) (tree);
};

/* Hooks related to types.  */

struct lang_hooks_for_types
{
  /* Return a new type (with the indicated CODE), doing whatever
     language-specific processing is required.  */
  tree (*make_type) (enum tree_code);

  /* Given MODE and UNSIGNEDP, return a suitable type-tree with that
     mode.  */
  tree (*type_for_mode) (enum machine_mode, int);

  /* Given PRECISION and UNSIGNEDP, return a suitable type-tree for an
     integer type with at least that precision.  */
  tree (*type_for_size) (unsigned, int);

  /* Given an integer type T, return a type like T but unsigned.
     If T is unsigned, the value is T.  */
  tree (*unsigned_type) (tree);

  /* Given an integer type T, return a type like T but signed.
     If T is signed, the value is T.  */
  tree (*signed_type) (tree);

  /* Return a type the same as TYPE except unsigned or signed
     according to UNSIGNEDP.  */
  tree (*signed_or_unsigned_type) (int, tree);

  /* Given a type, apply default promotions to unnamed function
     arguments and return the new type.  Return the same type if no
     change.  Required by any language that supports variadic
     arguments.  The default hook aborts.  */
  tree (*type_promotes_to) (tree);

  /* Register TYPE as a builtin type with the indicated NAME.  The
     TYPE is placed in the outermost lexical scope.  The semantics
     should be analogous to:

       typedef TYPE NAME;

     in C.  The default hook ignores the declaration.  */
  void (*register_builtin_type) (tree, const char *);

  /* This routine is called in tree.c to print an error message for
     invalid use of an incomplete type.  VALUE is the expression that
     was used (or 0 if that isn't known) and TYPE is the type that was
     invalid.  */
  void (*incomplete_type_error) (tree value, tree type);
};

/* Language hooks related to decls and the symbol table.  */

struct lang_hooks_for_decls
{
  /* Enter a new lexical scope.  Argument is always zero when called
     from outside the front end.  */
  void (*pushlevel) (int);

  /* Exit a lexical scope and return a BINDING for that scope.
     Takes three arguments:
     KEEP -- nonzero if there were declarations in this scope.
     REVERSE -- reverse the order of decls before returning them.
     FUNCTIONBODY -- nonzero if this level is the body of a function.  */
  tree (*poplevel) (int, int, int);

  /* Returns nonzero if we are in the global binding level.  Ada
     returns -1 for an undocumented reason used in stor-layout.c.  */
  int (*global_bindings_p) (void);

  /* Insert BLOCK at the end of the list of subblocks of the
     current binding level.  This is used when a BIND_EXPR is expanded,
     to handle the BLOCK node inside the BIND_EXPR.  */
  void (*insert_block) (tree);

  /* Set the BLOCK node for the current scope level.  */
  void (*set_block) (tree);

  /* Function to add a decl to the current scope level.  Takes one
     argument, a decl to add.  Returns that decl, or, if the same
     symbol is already declared, may return a different decl for that
     name.  */
  tree (*pushdecl) (tree);

  /* Returns the chain of decls so far in the current scope level.  */
  tree (*getdecls) (void);

  /* Returns a chain of TYPE_DECLs for built-in types.  */
  tree (*builtin_type_decls) (void);

  /* Returns true when we should warn for an unused global DECL.
     We will already have checked that it has static binding.  */
  bool (*warn_unused_global) (tree);

  /* Obtain a list of globals and do final output on them at end
     of compilation */
  void (*final_write_globals) (void);

  /* Do necessary preparations before assemble_variable can proceed.  */
  void (*prepare_assemble_variable) (tree);

  /* True if this decl may be called via a sibcall.  */
  bool (*ok_for_sibcall) (tree);
};

/* Language-specific hooks.  See langhooks-def.h for defaults.  */

struct lang_hooks
{
  /* String identifying the front end.  e.g. "GNU C++".  */
  const char *name;

  /* sizeof (struct lang_identifier), so make_node () creates
     identifier nodes long enough for the language-specific slots.  */
  size_t identifier_size;

  /* Determines the size of any language-specific 'x' or 'c' nodes.
     Since it is called from make_node, the only information available
     is the tree code.  Expected to abort on unrecognized codes.  */
  size_t (*tree_size) (enum tree_code);

  /* The first callback made to the front end, for simple
     initialization needed before any calls to handle_option.  Return
     the language mask to filter the switch array with.  */
  unsigned int (*init_options) (unsigned int argc, const char **argv);

  /* Callback used to perform language-specific initialization for the
     global diagnostic context structure.  */
  void (*initialize_diagnostics) (struct diagnostic_context *);

  /* Handle the switch CODE, which has real type enum opt_code from
     options.h.  If the switch takes an argument, it is passed in ARG
     which points to permanent storage.  The handler is responsible for
     checking whether ARG is NULL, which indicates that no argument
     was in fact supplied.  For -f and -W switches, VALUE is 1 or 0
     for the positive and negative forms respectively.

     Return 1 if the switch is valid, 0 if invalid, and -1 if it's
     valid and should not be treated as language-independent too.  */
  int (*handle_option) (size_t code, const char *arg, int value);

  /* Return false to use the default complaint about a missing
     argument, otherwise output a complaint and return true.  */
  bool (*missing_argument) (const char *opt, size_t code);

  /* Called when all command line options have been parsed to allow
     further processing and initialization

     Should return true to indicate that a compiler back-end is
     not required, such as with the -E option.

     If errorcount is nonzero after this call the compiler exits
     immediately and the finish hook is not called.  */
  bool (*post_options) (const char **);

  /* Called after post_options to initialize the front end.  Return
     false to indicate that no further compilation be performed, in
     which case the finish hook is called immediately.  */
  bool (*init) (void);

  /* Called at the end of compilation, as a finalizer.  */
  void (*finish) (void);

  /* Parses the entire file.  The argument is nonzero to cause bison
     parsers to dump debugging information during parsing.  */
  void (*parse_file) (int);

  /* Called immediately after parsing to clear the binding stack.  */
  void (*clear_binding_stack) (void);

  /* Called to obtain the alias set to be used for an expression or type.
     Returns -1 if the language does nothing special for it.  */
  HOST_WIDE_INT (*get_alias_set) (tree);

  /* Called with an expression that is to be processed as a constant.
     Returns either the same expression or a language-independent
     constant equivalent to its input.  */
  tree (*expand_constant) (tree);

  /* Called by expand_expr for language-specific tree codes.
     Fourth argument is actually an enum expand_modifier.  */
  rtx (*expand_expr) (tree, rtx, enum machine_mode, int, rtx *);

  /* Prepare expr to be an argument of a TRUTH_NOT_EXPR or other logical
     operation.

     This preparation consists of taking the ordinary representation
     of an expression expr and producing a valid tree boolean
     expression describing whether expr is nonzero.  We could simply
     always do build_binary_op (NE_EXPR, expr, integer_zero_node, 1),
     but we optimize comparisons, &&, ||, and !.

     The result should be an expression of boolean type (if not an
     error_mark_node).  */
  tree (*truthvalue_conversion) (tree);

  /* Hook called by safe_from_p for language-specific tree codes.  It is
     up to the language front-end to install a hook if it has any such
     codes that safe_from_p needs to know about.  Since same_from_p will
     recursively explore the TREE_OPERANDs of an expression, this hook
     should not reexamine those pieces.  This routine may recursively
     call safe_from_p; it should always pass `0' as the TOP_P
     parameter.  */
  int (*safe_from_p) (rtx, tree);

  /* Function to finish handling an incomplete decl at the end of
     compilation.  Default hook is does nothing.  */
  void (*finish_incomplete_decl) (tree);

  /* Function used by unsafe_for_reeval.  A non-negative number is
     returned directly from unsafe_for_reeval, a negative number falls
     through.  The default hook returns a negative number.  */
  int (*unsafe_for_reeval) (tree);

  /* Mark EXP saying that we need to be able to take the address of
     it; it should not be allocated in a register.  Return true if
     successful.  */
  bool (*mark_addressable) (tree);

  /* Hook called by staticp for language-specific tree codes.  */
  int (*staticp) (tree);

  /* Replace the DECL_LANG_SPECIFIC data, which may be NULL, of the
     DECL_NODE with a newly GC-allocated copy.  */
  void (*dup_lang_specific_decl) (tree);

  /* Called before its argument, an UNSAVE_EXPR, is to be
     unsaved.  Modify it in-place so that all the evaluate only once
     things are cleared out.  */
  tree (*unsave_expr_now) (tree);

  /* Called by expand_expr to build and return the cleanup-expression
     for the passed TARGET_EXPR.  Return NULL if there is none.  */
  tree (*maybe_build_cleanup) (tree);

  /* Set the DECL_ASSEMBLER_NAME for a node.  If it is the sort of
     thing that the assembler should talk about, set
     DECL_ASSEMBLER_NAME to an appropriate IDENTIFIER_NODE.
     Otherwise, set it to the ERROR_MARK_NODE to ensure that the
     assembler does not talk about it.  */
  void (*set_decl_assembler_name) (tree);

  /* Return nonzero if fold-const is free to use bit-field
     optimizations, for instance in fold_truthop().  */
  bool (*can_use_bit_fields_p) (void);

  /* Nonzero if TYPE_READONLY and TREE_READONLY should always be honored.  */
  bool honor_readonly;

  /* Nonzero if this front end does not generate a dummy BLOCK between
     the outermost scope of the function and the FUNCTION_DECL.  See
     is_body_block in stmt.c, and its callers.  */
  bool no_body_blocks;

  /* The front end can add its own statistics to -fmem-report with
     this hook.  It should output to stderr.  */
  void (*print_statistics) (void);

  /* Called by print_tree when there is a tree of class 'x' that it
     doesn't know how to display.  */
  lang_print_tree_hook print_xnode;

  /* Called to print language-dependent parts of a class 'd', class
     't', and IDENTIFIER_NODE nodes.  */
  lang_print_tree_hook print_decl;
  lang_print_tree_hook print_type;
  lang_print_tree_hook print_identifier;

  /* Computes the name to use to print a declaration.  DECL is the
     non-NULL declaration in question.  VERBOSITY determines what
     information will be printed: 0: DECL_NAME, demangled as
     necessary.  1: and scope information.  2: and any other
     information that might be interesting, such as function parameter
     types in C++.  */
  const char *(*decl_printable_name) (tree decl, int verbosity);

  /* Given a CALL_EXPR, return a function decl that is its target.  */
  tree (*lang_get_callee_fndecl) (tree);

  /* Called by report_error_function to print out function name.  */
  void (*print_error_function) (struct diagnostic_context *, const char *);

  /* Called from expr_size to calculate the size of the value of an
     expression in a language-dependent way.  Returns a tree for the size
     in bytes.  A frontend can call lhd_expr_size to get the default
     semantics in cases that it doesn't want to handle specially.  */
  tree (*expr_size) (tree);

  /* Called from uninitialized_vars_warning to find out if a variable is
     uninitialized based on DECL_INITIAL.  */
  bool (*decl_uninit) (tree);

  /* Pointers to machine-independent attribute tables, for front ends
     using attribs.c.  If one is NULL, it is ignored.  Respectively, a
     table of attributes specific to the language, a table of
     attributes common to two or more languages (to allow easy
     sharing), and a table of attributes for checking formats.  */
  const struct attribute_spec *attribute_table;
  const struct attribute_spec *common_attribute_table;
  const struct attribute_spec *format_attribute_table;

  /* Function-related language hooks.  */
  struct lang_hooks_for_functions function;

  struct lang_hooks_for_tree_inlining tree_inlining;

  struct lang_hooks_for_callgraph callgraph;

  struct lang_hooks_for_tree_dump tree_dump;

  struct lang_hooks_for_decls decls;

  struct lang_hooks_for_types types;

  struct lang_hooks_for_rtl_expansion rtl_expand;

  /* Whenever you add entries here, make sure you adjust langhooks-def.h
     and langhooks.c accordingly.  */
};

/* Each front end provides its own.  */
extern const struct lang_hooks lang_hooks;

#endif /* GCC_LANG_HOOKS_H */
