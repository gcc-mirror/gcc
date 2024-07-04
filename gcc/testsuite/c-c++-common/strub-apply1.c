/* { dg-do compile } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target strub } */

void __attribute__ ((__strub__ ("callable")))
apply_function (void *args)
{
  __builtin_apply (0, args, 0);
}

void __attribute__ ((__strub__ ("internal")))
apply_args (int i, int j, double d)
{
  void *args = __builtin_apply_args ();
  apply_function (args);
}
