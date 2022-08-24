
int not_a_fn __attribute__ ((fd_arg(1))); /* { dg-warning "'fd_arg' attribute only applies to function types" } */

void f (char *p) __attribute__ ((fd_arg(1))); /* { dg-warning "'fd_arg' attribute argument value '1' refers to parameter type 'char ?\\\*'" } */


int not_a_fn_b __attribute__ ((fd_arg_read(1))); /* { dg-warning "'fd_arg_read' attribute only applies to function types" } */

void g (char *p) __attribute__ ((fd_arg_read(1))); /* { dg-warning "'fd_arg_read' attribute argument value '1' refers to parameter type 'char ?\\\*'" } */


int not_a_fn_c __attribute__ ((fd_arg_write(1))); /* { dg-warning "'fd_arg_write' attribute only applies to function types" } */

void f (char *p) __attribute__ ((fd_arg_write(1))); /* { dg-warning "'fd_arg_write' attribute argument value '1' refers to parameter type 'char ?\\\*'" } */


void fn_a (int fd) __attribute__ ((fd_arg(0))); /* { dg-warning "'fd_arg' attribute argument value '0' does not refer to a function parameter" } */
void fd_a_1 (int fd) __attribute__ ((fd_arg("notint"))); /* { dg-warning "'fd_arg' attribute argument has type ('char\\\[7\\\]'|'const char\\\*')" } */
