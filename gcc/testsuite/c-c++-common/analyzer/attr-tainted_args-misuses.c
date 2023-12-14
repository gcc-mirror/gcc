int not_a_fn __attribute__ ((tainted_args)); /* { dg-warning "'tainted_args' attribute ignored; valid only for functions and function pointer fields" } */

struct s
{
  int f __attribute__ ((tainted_args)); /* { dg-warning "'tainted_args' attribute ignored; field must be a function pointer" } */
};
