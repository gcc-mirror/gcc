/* This file is used by test-combination.c and test-threads.c to
   bring all of the non-failing test cases into one source file,
   renaming each "create_code" and "verify_code" hook so that they
   each have unique name.  */

/* Include various other test cases, defining COMBINED_TEST so that
   harness.h doesn't duplicate copes of e.g. main, and renaming the
   hooks provided by each test case.  */
#define COMBINED_TEST

/* test-accessing-struct.c */
#define create_code create_code_accessing_struct
#define verify_code verify_code_accessing_struct
#include "test-accessing-struct.c"
#undef create_code
#undef verify_code

/* test-accessing-union.c */
#define create_code create_code_accessing_union
#define verify_code verify_code_accessing_union
#include "test-accessing-union.c"
#undef create_code
#undef verify_code

/* test-arith-overflow.c */
#define create_code create_code_arith_overflow
#define verify_code verify_code_arith_overflow
#include "test-arith-overflow.c"
#undef create_code
#undef verify_code

/* test-array-as-pointer.c */
#define create_code create_code_array_as_pointer
#define verify_code verify_code_array_as_pointer
#include "test-array-as-pointer.c"
#undef create_code
#undef verify_code

/* test-arrays.c */
#define create_code create_code_arrays
#define verify_code verify_code_arrays
#include "test-arrays.c"
#undef create_code
#undef verify_code

/* test-calling-external-function.c */
#define create_code create_code_calling_external_function
#define verify_code verify_code_calling_external_function
#include "test-calling-external-function.c"
#undef create_code
#undef verify_code

/* test-calling-function-ptr.c */
#define create_code create_code_calling_function_ptr
#define verify_code verify_code_calling_function_ptr
#include "test-calling-function-ptr.c"
#undef create_code
#undef verify_code

/* test-compound-assignment.c */
#define create_code create_code_compound_assignment
#define verify_code verify_code_compound_assignment
#include "test-compound-assignment.c"
#undef create_code
#undef verify_code

/* test-constants.c */
#define create_code create_code_constants
#define verify_code verify_code_constants
#include "test-constants.c"
#undef create_code
#undef verify_code

/* test-debug-strings.c */
#define create_code create_code_debug_strings
#define verify_code verify_code_debug_strings
#include "test-debug-strings.c"
#undef create_code
#undef verify_code

/* test-dot-product.c */
#define create_code create_code_dot_product
#define verify_code verify_code_dot_product
#include "test-dot-product.c"
#undef create_code
#undef verify_code

/* test-error-*.c: We don't use these test cases, since they deliberately
   introduce errors, which we don't want here.  */

/* test-expressions.c */
#define create_code create_code_expressions
#define verify_code verify_code_expressions
#include "test-expressions.c"
#undef create_code
#undef verify_code

/* test-extra-options.c: We don't use this one, since the extra options
   affect the whole context.  */

/* test-factorial.c */
#define create_code create_code_factorial
#define verify_code verify_code_factorial
#include "test-factorial.c"
#undef create_code
#undef verify_code

/* test-fibonacci.c */
#define create_code create_code_fibonacci
#define verify_code verify_code_fibonacci
#include "test-fibonacci.c"
#undef create_code
#undef verify_code

/* test-functions.c */
#define create_code create_code_functions
#define verify_code verify_code_functions
#include "test-functions.c"
#undef create_code
#undef verify_code

/* test-hello-world.c */
#define create_code create_code_hello_world
#define verify_code verify_code_hello_world
#include "test-hello-world.c"
#undef create_code
#undef verify_code

/* test-linked-list.c */
#define create_code create_code_linked_list
#define verify_code verify_code_linked_list
#include "test-linked-list.c"
#undef create_code
#undef verify_code

/* test-long-names.c */
#define create_code create_code_long_names
#define verify_code verify_code_long_names
#include "test-long-names.c"
#undef create_code
#undef verify_code

/* test-quadratic.c */
#define create_code create_code_quadratic
#define verify_code verify_code_quadratic
#include "test-quadratic.c"
#undef create_code
#undef verify_code

/* test-nested-loops.c */
#define create_code create_code_nested_loop
#define verify_code verify_code_nested_loop
#include "test-nested-loops.c"
#undef create_code
#undef verify_code

/* test-reading-struct.c */
#define create_code create_code_reading_struct
#define verify_code verify_code_reading_struct
#include "test-reading-struct.c"
#undef create_code
#undef verify_code

/* test-string-literal.c */
#define create_code create_code_string_literal
#define verify_code verify_code_string_literal
#include "test-string-literal.c"
#undef create_code
#undef verify_code

/* test-sum-of-squares.c */
#define create_code create_code_sum_of_squares
#define verify_code verify_code_sum_of_squares
#include "test-sum-of-squares.c"
#undef create_code
#undef verify_code

/* test-switch.c */
#define create_code create_code_switch
#define verify_code verify_code_switch
#include "test-switch.c"
#undef create_code
#undef verify_code

/* test-types.c */
#define create_code create_code_types
#define verify_code verify_code_types
#include "test-types.c"
#undef create_code
#undef verify_code

/* test-using-global.c */
#define create_code create_code_using_global
#define verify_code verify_code_using_global
#include "test-using-global.c"
#undef create_code
#undef verify_code

/* test-validly-unreachable-block.c: We don't use this one, since the use
   of gcc_jit_context_set_bool_allow_unreachable_blocks affects the whole
   context.  */

/* test-volatile.c */
#define create_code create_code_volatile
#define verify_code verify_code_volatile
#include "test-volatile.c"
#undef create_code
#undef verify_code

/* Now expose the individual testcases as instances of this struct.  */

struct testcase
{
  const char *m_name;
  void (*m_hook_to_create_code) (gcc_jit_context *ctxt,
				 void * user_data);
  void (*m_hook_to_verify_code) (gcc_jit_context *ctxt,
				 gcc_jit_result *result);
};

const struct testcase testcases[] = {
  {"accessing_struct",
   create_code_accessing_struct,
   verify_code_accessing_struct},
  {"accessing_union",
   create_code_accessing_union,
   verify_code_accessing_union},
  {"arith_overflow",
   create_code_arith_overflow,
   verify_code_arith_overflow},
  {"array_as_pointer",
   create_code_array_as_pointer,
   verify_code_array_as_pointer},
  {"arrays",
   create_code_arrays,
   verify_code_arrays},
  {"calling_external_function",
   create_code_calling_external_function,
   verify_code_calling_external_function},
  {"calling_function_ptr",
   create_code_calling_function_ptr,
   verify_code_calling_function_ptr},
  {"compound_assignment",
   create_code_compound_assignment,
   verify_code_compound_assignment},
  {"constants",
   create_code_constants,
   verify_code_constants},
  {"debug_strings",
   create_code_debug_strings,
   verify_code_debug_strings},
  {"dot_product",
   create_code_dot_product,
   verify_code_dot_product},
  {"expressions",
   create_code_expressions,
   verify_code_expressions},
  {"factorial",
   create_code_factorial,
   verify_code_factorial},
  {"fibonacci",
   create_code_fibonacci,
   verify_code_fibonacci},
  {"functions",
   create_code_functions,
   verify_code_functions},
  {"hello_world",
   create_code_hello_world,
   verify_code_hello_world},
  {"linked_list",
   create_code_linked_list,
   verify_code_linked_list},
  {"long_names",
   create_code_long_names,
   verify_code_long_names},
  {"quadratic",
   create_code_quadratic,
   verify_code_quadratic},
  {"nested_loop",
   create_code_nested_loop,
   verify_code_nested_loop},
  {"reading_struct ",
   create_code_reading_struct ,
   verify_code_reading_struct },
  {"string_literal",
   create_code_string_literal,
   verify_code_string_literal},
  {"sum_of_squares",
   create_code_sum_of_squares,
   verify_code_sum_of_squares},
  {"switch",
   create_code_switch,
   verify_code_switch},
  {"types",
   create_code_types,
   verify_code_types},
  {"using_global",
   create_code_using_global,
   verify_code_using_global},
  {"volatile",
   create_code_volatile,
   verify_code_volatile}
};

const int num_testcases = (sizeof (testcases) / sizeof (testcases[0]));
