/* { dg-do compile } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target strub } */

extern void __attribute__ ((__strub__))
apply_function (void *args);

void __attribute__ ((__strub__))
apply_args (int i, int j, double d) /* { dg-error "selected" } */
{
  void *args = __builtin_apply_args (); /* { dg-message "does not support" } */
  apply_function (args);
}
