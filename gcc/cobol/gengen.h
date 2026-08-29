/*
 * Copyright (c) 2021-2026 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef __GENGEN_H
#define __GENGEN_H

// Note how the definitions of IF and WHILE lets you use them as
// IF(a,b,c) and WHILE(a,b,c) with no semicolon.
// And, yes, I see that ELSE, ENDIF, and WEND are all the same.  Sometimes
// looks *are* important, and the multiple definitions make things easier
// to understand.

#define IF_CONDITION(a) gg_create_true_false_statement_lists(a);
#define IF(a,b,c) gg_if((a),(b),(c));
#define ELSE current_function->statement_list_stack.pop_back();
#define ENDIF current_function->statement_list_stack.pop_back();
#define WHILE(a,b,c) gg_while((a),(b),(c));
#define WEND current_function->statement_list_stack.pop_back();

// mnemonics for variable types:

#define VOID         void_type_node
#define BOOL         boolean_type_node
#define CHAR         char_type_node
#define SCHAR        signed_char_type_node
#define SCHAR_P      build_pointer_type(SCHAR)
#define UCHAR        unsigned_char_type_node
#define SHORT        short_integer_type_node
#define SHORT_P      build_pointer_type(short_integer_type_node)
#define USHORT       short_unsigned_type_node
#define WCHAR        short_unsigned_type_node
#define INT          integer_type_node
#define INT_P        build_pointer_type(integer_type_node)
#define UINT         unsigned_type_node
#define LONG         long_integer_type_node
#define ULONG        long_unsigned_type_node
#define LONGLONG     long_long_integer_type_node
#define UINT64       uint64_type_node
#define SIZE_T       size_type_node
#define SIZE_T_P     (build_pointer_type(SIZE_T))
#define SSIZE_T      ptrdiff_type_node
#define INT128       intTI_type_node
#define UINT128      unsigned_intTI_type_node
#define FLOAT        float32_type_node
#define DOUBLE       float64_type_node
#define LONGDOUBLE   long_double_type_node
#define FLOAT128     float128_type_node
#define VOID_P       ptr_type_node
#define VOID_P_P     (build_pointer_type(VOID_P))
#define CHAR_P       char_ptr_type_node
#define CONST_CHAR_P const_char_ptr_type_node
#define UCHAR_P      uchar_ptr_type_node
#define WCHAR_P      wchar_ptr_type_node
#define FILE_P       fileptr_type_node

#define SIZE128 (16) // In bytes

/*  Explanatory note for vs_file_static variables

    In a C program, you can have this variable declaration outside of a
    function:

      static const int intvar = 12321;

    It will be visible to any function that follows.  After several days of
    experimentation and research, I found I was unable to duplicate this
    behavior in the GCOBOL code generator.  I simply wasn't able to reverse
    engineer whatever magical incantations are necessary to declare and define]
    variables on the translation unit level rather than on the function level.

    Having reached the point where the structural integrity of my desk was being
    threatened by the repeated percussive strikes from my forehead, I turned my
    attention to an equivalent workaround.

    On the assembly language level, there is no fundamental way of making a
    variable visible to only a specific function.  So, to distinguish between
    two non-global variables named "fred" in two different functions, the C
    compiler appends a dot and a number, with the "number" being different for
    the two functions.

    The GCOBOL compiler has been doing just that.  So, to implement a
    vs_file_static variable, I treat it just like a vs_static variable, but
    without appending a differentiator.

    */

enum gg_variable_scope_t {
  // These scopes reference the GCC world rather than the COBOL world.
  // This is in contrast to the cbl_field_attr_t bits, where external_e means
  // the variable had the COBOL EXTERNAL clause, which causes a variable to
  // have a weak cblc_field_t and a common cblc_field_t::data.
  vs_stack,        // An automatic variable.
  vs_static,       // A static variable of function scope.
  vs_file_static,  // static variable of file scope.
  vs_weak,         // A "weak" variable.
  vs_common,       // A COMMON variable.
  vs_global,       // A global variable, e.g. "int xxx;" in C
  vs_extern,       // A declaration for a global; "extern int xxx;"
};

struct gg_function_t
    {
    // Nomenclature Alert:  The "function" in gg_function_t was chosen
    // originally because a PROGRAM-ID is implemented as a C-style "function",
    // and there are numerous tree variables that refer to "functions".
    // Eventually the COBOL compiler grew to handle not just COBOL PROGRAM-ID
    // "programs", but also user-defined COBOL FUNCTION-ID "functions".  This
    // inevitably is confusing.  Sorry about that.

    // This structure contains state variables for a single function.

    bool initialized; // Starts off false; used for one-time initialization

    const char *our_unmangled_name;   // This is the original name
    const char *our_name;             // This is our mangled name
    tree        function_address;
    size_t our_symbol_table_index;
    bool has_initial;     // The program-id has the INITIAL clause.
    bool has_recursive;   // The program-id has the RECURSIVE clause.

    // The function_decl is fundamental to many, many things
    tree function_decl;

    // Every function has a context, wherein temporary variables get created
    // and whose names won't collide with the names in other function.

    // But it is often necessary to create subcontexts, which inherit names from
    // its parent function, but can reuse names, and create new ones, without
    // collisions.  Each context gets its own bind_expr, each bind_expr points
    // to its own block.  So, to create subcontexts, we need to know which
    // bind_expr we add variable declarations to.
    std::vector<tree> bind_expr_stack;

    // Every function has a statement list.  But there can be statements
    // that consist of statement lists.  This happens when building IF
    // statements (TRUE gets its own list, as does FALSE) and WHILE statements
    // (where the execution block is a statement list.  This stack enables that
    // to happen cleanly, so the programmer doesn't have to be concerned about
    // which list is being built.

    // Note that the gg_statement_list_stack can grow larger than the
    // current_function->bind_expr_stack stack, because
    // there are times -- like inside of WHILE() and IF constructs -- where we
    // push onto the statement_list_stack and even create new bind_expr nodes,
    // but don't need a full new context.  But every new context gets a new
    // statement list, and when
    // current_function->bind_expr_stack is popped,
    // statement_list_stack is popped, too.
    std::vector<tree> statement_list_stack;

    // COBOL sections and paragraphs are handled identically; it's the context
    // that makes them different:  PROGRAMS contain SECTIONS, and SECTIONS
    // contain paragraphs.  I call both SECTIONS and PARAGRAPHS "procs"

    // At any given moment, there is one "current section" and one "current
    // paragraph".
    struct cbl_proc_t *current_section;
    struct cbl_proc_t *current_paragraph;

    // This carries an indirect pointer reference to RETURN-CODE
    tree var_decl_return;

    tree first_time_through;

    tree skip_init_goto;
    tree skip_init_label;

    // We use context_count to detect a mismatch between gg_push_context() and
    // gg_pop_context calls, which should be equal at the point gimplify is
    // invoked:
    int context_count;

    // When a function is called, it comes with zero to N parameters on the
    // stack.  We treat it as variadic; see parser_division(PROCEDURE) to see
    // how we pick up the N values on the stack:
    tree formal_parameters;

    // When parser_division(PROCEDURE) is called, it provides a cbl_field_t
    // *returning parameter.  We stash it here; it's used during parser_exit()
    // to provide the data for the program's return value.
    cbl_field_t *returning;  // This one is on the stack, like a LOCAL-STORAGE

    size_t program_id_number;   // Used to give static variables
    //                          // a unique .<n> suffix

    // There are two types of nesting.  COBOL nesting is implemented in a
    // logical way: All programs are siblings, with the context being the source
    // code module.  The nested aspect is not reflected in the GENERIC tree.

    // This variable, which appears on the stack, contains the exit_address
    // for the terminating proc of a PERFORM A or PERFORM A THROUGH B
    tree perform_exit_address;

    // This variable is a pointer to the first declarative section of this
    // program-id/function.  It's used in when creating the linked list of
    // declaratives, because the last declarative of a nested function links
    // back to the first declarative of its immediate parent.
    tree first_declarative_section;

    // is_function is true when this structure is describing a COBOL FUNCTION-ID
    // and is false for a PROGRAM-ID
    bool is_function;

    // This integer is initially set to one when this function is called by
    // our generated main().  It gets incremented by 1 when the routine is
    // re-entered:  main() -> us -> B -> us
    // When processing EXIT PROGRAM, if the counter is greater then 1, it is
    // decremented and a return is created.  When the counter is 1, the
    // EXIT program is treated as a CONTINUE.
    tree called_by_main_counter;

    // We used to use indirect jumps to implement "pseudo-return" from PERFORM
    // <proc> statements.  But that led to N-squared complexity in the Control
    // Flow Graph, because the middle-end can't make assumptions about the
    // target of the JMP *%rax; as far as the middle-end is concerned *any*
    // label in the program could be a target.
    //
    // We are now reducing the complexity to linear by using a switch()
    // statement on an identifier.  The following map collects the indexes
    // used for the switch statement.

    // In order to reduce the complexity of the Control Flow Graph, we build a
    // an array of all paragraphs. For each such paragraph, we also build a
    // vector of of the return locations of PERFORM statements that target it.
    // Those tables are used to create one dispatching switch statement per
    // paragraph.  Each switch statements has exactly one CASE for each PERFORM
    // of the paragraph, each CASE contains a GOTO the return location of that
    // PERFORM.
    //
    // The map uses the paragraph's proc_t * as a key.  The payload is the
    // index into the vector of vectors.

    std::vector<void *> list_of_procedures;

    // The following is an SIZE_T variable node.  It is set by every PERFORM
    // statement to establish where the end-of-paragraph dispatch switch picks
    // a GOTO statement for the return.
    tree pseudo_return_index;

    // The ENTRY statement creates alternative entry point to a program-id. We
    // implement that as a SWITCH_EXPR.  At the main entry point for a
    // program-id, we check to see if an alternative entry point has been
    // established.  If so, we jump to the SWITCH statement which dispatches
    // execution to the alternate location.
    tree entry_switch_goto;
    tree entry_switch_label;
    std::vector<tree> entry_goto_expressions;
    bool alphabet_in_use;
    };

struct cbl_translation_unit_t
    {
    // GCC calls a source file a "translation unit".  This structure contains
    // all of the information needed by and for a translation unit.  There
    // probably should be one, and only one, of these instantiated by the COBOL
    // front end.

    // Every function in this code module gets this translation_unit_decl
    // as its context.  This node is built in parse_enter_file()
    tree trans_unit_decl;

    // This is the filename of this trans_unit_decl
    const char *filename;

    // This is the stack of function_decls in this translation unit; each
    // call to parser_enter_program() pushes onto this stack; each call to
    // parser_end_program() pops it.
    std::vector<struct gg_function_t> function_stack;

    // This is where we keep var_decls because of my inability to figure out how
    // to tell the compiler to create data definitions for translation_unit_decl
    // variables:
    std::unordered_map<std::string, tree> trans_unit_var_decls;

    // This map is to make chaining BLOCKs an O(1) operation.
    std::unordered_map<tree, tree> block_var_tails;
    };

extern struct cbl_translation_unit_t gg_trans_unit;

#define current_function (&gg_trans_unit.function_stack.back())

extern GTY(()) tree char_nodes[256]         ;
extern GTY(()) tree pvoid_type_node         ;
extern GTY(()) tree integer_minusone_node   ;
extern GTY(()) tree integer_two_node        ;
extern GTY(()) tree integer_eight_node      ;
extern GTY(()) tree size_t_zero_node        ;
extern GTY(()) tree int128_zero_node        ;
extern GTY(()) tree int128_five_node        ;
extern GTY(()) tree int128_ten_node         ;
extern GTY(()) tree bool_true_node          ;
extern GTY(()) tree bool_false_node         ;
extern GTY(()) tree char_ptr_type_node      ;
extern GTY(()) tree const_char_ptr_type_node;
extern GTY(()) tree uchar_ptr_type_node     ;
extern GTY(()) tree wchar_ptr_type_node     ;
extern GTY(()) tree long_double_ten_node    ;
extern GTY(()) tree sizeof_size_t           ;
extern GTY(()) tree sizeof_pointer          ;

// These routines happen when beginning to process a new file, which is also
// known, in GCC, as a "translation unit"
extern void gg_build_translation_unit(const char *filename);

// For an expression type to actually be implemented in the target
// runtime binary, it has to find its way onto a statement list.  (Or be used
// as the second operand of a modify_expr, and so on.)
extern void gg_append_statement(tree stmt);
//// extern void gg_insert_statement(struct tree_stmt_iterator *tsi, tree stmt);

// For variables:
extern void gg_append_var_decl(tree var);

// type cast
extern tree gg_float(tree float_type, tree integer_var);
extern tree gg_trunc(tree integer_type, tree float_var);
extern tree gg_cast(tree type, tree var);

// Assignment, that is to say, A = B
extern tree gg_assign(tree dest, const tree source);

// Create constructor for a record_type
extern void gg_structure_type_constructor(tree record_decl, ...);

extern tree gg_find_field_in_struct(const tree var_decl, const char *field_name);
extern tree gg_struct_field_ref(const tree struct_decl, const char *field);

// Generalized variable declarer.  This doesn't create storage
extern tree gg_declare_variable(tree type_decl,
                                const char *name=NULL,
                                tree initial_value=NULL_TREE,
                                gg_variable_scope_t vs_scope=vs_stack,
                                bool *already_defined = NULL);
extern tree gg_define_from_declaration(tree var_decl);

// Generalized variable definers.  These create storage
extern tree gg_define_variable(tree type_decl);
extern tree gg_define_variable(tree type_decl, tree initial_value);
extern tree gg_define_variable(tree type_decl, ssize_t initial_value);
extern tree gg_define_variable(tree type_decl, gg_variable_scope_t vs_scope);
extern tree gg_define_variable(tree type_decl,
                               const char *name,
                               gg_variable_scope_t vs_scope=vs_stack);
extern tree gg_define_variable(tree type_decl,
                               const char *name,
                               gg_variable_scope_t vs_scope,
                               tree initial_value);

// address_of operator; equivalent of C "&var_decl"
extern tree gg_get_address_of(const tree var_decl); // For scalars
// equivalent of C "&array[0]"
extern tree gg_pointer_to_array(tree array);        // For arrays
extern tree gg_get_address(const tree var_decl);

// Array creation and access:
extern tree gg_define_array(tree type_decl, size_t size);
extern tree gg_define_array(tree type_decl, const char *name, size_t size);
extern tree gg_define_array(tree type_decl, size_t size, gg_variable_scope_t scope);
extern tree gg_define_array(tree type_decl, const char *name, size_t size, gg_variable_scope_t scope);

extern tree gg_array_value(tree pointer, tree offset=NULL_TREE);
extern tree gg_array_value(tree pointer, int N);

// Here are some unary operations
extern void gg_increment(tree var);
extern void gg_increment2(tree &var);
extern void gg_decrement(tree var);
extern void gg_decrement2(tree &var);
extern tree gg_negate(tree var);        // Two's complement negation
extern tree gg_bitwise_not(tree var);   // Bitwise inversion
extern tree gg_abs(tree var);           // Absolute value
extern tree gg_bswap(tree var);         // end-for-end byte swap

// And some binary operations:

extern tree gg_add(tree addend1, tree addend2);
extern tree gg_subtract(tree A, tree B);
extern tree gg_multiply(tree A, tree B);
extern tree gg_real_divide(tree A, tree B); // Floating point division
extern tree gg_divide(tree A, tree B);      // Integer division
extern tree gg_mod(tree A, tree B);
extern tree gg_lshift(tree A, tree B);
extern tree gg_rshift(tree A, tree B);
extern tree gg_bitwise_or(tree A, tree B);
extern tree gg_bitwise_xor(tree A, tree B);
extern tree gg_bitwise_and(tree A, tree B);

// Conditionals:  Use the IF() and WHILE() macros, which generated
// code that calls these functions.  Calling them yourself is
// probably a bad idea because there are stacks that have to be
// kept in the right states.

extern tree gg_build_relational_expression( tree operand_a,
        enum relop_t op,
        tree operand_b);
extern tree gg_build_logical_expression(tree operand_a,
        enum logop_t op,
        tree operand_b);

extern tree gg_logical_eq(tree a, tree b);
extern tree gg_logical_ne(tree a, tree b);
extern tree gg_logical_lt(tree a, tree b);
extern tree gg_logical_gt(tree a, tree b);
extern tree gg_logical_le(tree a, tree b);
extern tree gg_logical_ge(tree a, tree b);
extern tree gg_logical_and(tree a, tree b);
extern tree gg_logical_or(tree a, tree b);
extern tree gg_logical_xor(tree a, tree b);
extern tree gg_logical_not(tree a);
extern tree if_condition(tree cond,
                         tree expr_if_true,
                         tree expr_if_false,
                         tree type);
extern void gg_create_true_false_statement_lists(tree relational_expression);
extern void gg_while(tree operand_a, enum relop_t op, tree operand_b);
extern void gg_if(   tree operand_a, enum relop_t op, tree operand_b);

// Are are some system functions that can be useful.  gg_printf is
// particularly useful for generating run-time messages.  Actual run-time
// code is built using write(), because it allows for file descriptors and
// doesn't require null-terminated strings.

extern tree gg_get_function_address(tree return_type, const char *funcname);
extern void gg_printf(const char *format_string, ...);
extern tree gg_fprintf(tree fd, int nargs, const char *format_string, ...);
extern tree gg_read(tree fd, tree buf, tree count);
extern void gg_write(tree fd, tree buf, tree count);
extern void gg_memset(tree dest, const tree value, tree size);
extern tree gg_memchr(tree s, tree c, tree n);
extern tree gg_memcmp(const tree dest, const tree src, tree size);
extern void gg_memcpy(tree dest, const tree src, tree size);
extern void gg_memmove(tree dest, const tree src, tree size);
extern void gg_strcpy(tree char_star_A, tree char_star_B);
extern tree gg_strdup(tree char_star_A);
extern tree gg_strcmp(tree char_star_A, tree char_star_B);
extern tree gg_strncmp(tree char_star_A, tree char_star_B, tree size_t_N);

// Flow control inside a function
extern void gg_return(tree operand = NULL_TREE);

// These routines are the preamble and postamble that bracket everything else
extern tree gg_build_fn_decl(const char *funcname, tree fndecl_type);
extern tree gg_define_function( tree return_type,
                                const char *funcname,
                                const char *unmangled_name,
                                ...);
extern void chain_parameter_to_function( tree function_decl,
                                        const tree param_type,
                                        const char *name);
extern void gg_modify_function_type(tree function_decl, tree return_type);

extern void gg_finalize_function();
extern void gg_push_context();
extern void gg_pop_context();

// These are a generalized call constructor.  The first for when you just want
// the function called, because you don't care about the return value.  The others
// are for when you do need the return value.
extern tree gg_call_expr_list(tree return_type,
                              tree function_pointer,
                              int param_count, tree[]);

// The following is a garden-variety call, with known return type and known
// but in the case where the return value is unimportant.
extern void gg_call (tree return_type, const char *function_name, ...);
extern tree gg_call_expr(tree return_type, const char *function_name, ...);

// Returns a simple entangled goto/comefrom pair.  Used for things like
// IF/ELSE/ENDIF and WHILE/WEND
void gg_create_goto_pair(tree *goto_expr, tree *label_expr);
void gg_create_goto_pair(tree *goto_expr, tree *label_expr, const char *name);

// This more complex version is used for implementing SECTIONS and PARAGRAPHS.
void gg_create_goto_pair(   tree *goto_expr,
                            tree *label_expr,
                            tree *label_addr,
                            const char *name);
void gg_create_goto_pair(   tree *goto_expr,
                            tree *label_expr,
                            tree *label_addr);
void gg_create_goto_pair( tree *goto_expr,
                          tree *label_expr,
                          tree *label_addr,
                          tree *label_decl);

// Used for implementing SECTIONS and PARAGRAPHS.  When you have a
// void *pointer = &&label, gg_goto is the same as
//  goto *pointer
void gg_goto(tree pointer);

void gg_record_statement_list_start();
tree gg_record_statement_list_finish();

// Used to call system exit()
extern void gg_exit(tree exit_code);
extern void gg_abort();

extern tree gg_malloc(tree length);
extern tree gg_malloc(size_t length);
extern tree gg_realloc(tree base, tree length);
extern tree gg_realloc(tree base, size_t length);
extern void gg_free(tree pointer);
extern tree gg_strlen(tree psz);
extern size_t gg_sizeof(tree decl_node);

extern tree gg_array_of_field_pointers( const std::vector<const cbl_field_t *> &fields );
extern tree gg_array_of_bytes( size_t N, const unsigned char *values);
extern tree gg_indirect(tree pointer, tree byte_offset = NULL_TREE);
extern tree gg_indirect_i(tree pointer, size_t offset=0);
extern tree gg_string_literal(const char *string);

#define CURRENT_LINE_NUMBER (cobol_location().first_line)
extern location_t gg_token_location();
extern location_t current_token_location();
extern location_t current_location_minus_one();
extern void current_location_minus_one_clear();
extern void token_location_override(location_t loc);

extern tree gg_trans_unit_var_decl(const char *var_name);

extern tree gg_open(tree char_star_A, tree int_B);
extern tree gg_close(tree int_A);
extern tree gg_get_indirect_reference(tree pointer, tree offset);

extern void gg_insert_into_assembler(const char ach[]);
extern void gg_insert_into_assemblerf(const char *format, ...) ATTRIBUTE_PRINTF_1;

extern char *gg_show_type(tree type);
extern void gg_leaving_the_source_code_file();
extern tree gg_create_assembler_name(const char *cobol_name);
extern const char * label_decl_text_from_expr(tree expr);

#endif
