extern int not_a_function __attribute__((null_terminated_string_arg(1))); /* { dg-warning "'null_terminated_string_arg' attribute only applies to function types" } */

extern void no_arg (void) __attribute__((null_terminated_string_arg)); /* { dg-error "wrong number of arguments specified for 'null_terminated_string_arg' attribute" } */

extern void arg_idx_not_an_int (int) __attribute__((null_terminated_string_arg ("foo"))); /* { dg-warning "'null_terminated_string_arg' attribute argument has type" } */

extern void arg_not_a_pointer (int) __attribute__((null_terminated_string_arg (1))); /* { dg-warning "'null_terminated_string_arg' attribute argument value '1' refers to parameter type 'int'" } */

extern void arg_not_a_char_pointer (int) __attribute__((null_terminated_string_arg (1))); /* { dg-warning "'null_terminated_string_arg' attribute argument value '1' refers to parameter type 'int'" } */

extern void arg_idx_too_low (const char *) __attribute__((null_terminated_string_arg (0))); /* { dg-warning "'null_terminated_string_arg' attribute argument value '0' does not refer to a function parameter" } */

extern void arg_idx_too_high (const char *) __attribute__((null_terminated_string_arg (2))); /* { dg-warning "'null_terminated_string_arg' attribute argument value '2' exceeds the number of function parameters 1" } */

extern void valid_non_const (char *) __attribute__((null_terminated_string_arg (1)));
extern void valid_const (const char *) __attribute__((null_terminated_string_arg (1)));
