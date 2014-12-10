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

/* test-volatile.c */
#define create_code create_code_volatile
#define verify_code verify_code_volatile
#include "test-volatile.c"
#undef create_code
#undef verify_code
