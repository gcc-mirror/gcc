/* { dg-do compile } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target strub } */

void __attribute__ ((__strub__))
apply_function (void *args)
{
  __builtin_apply (0, args, 0); /* { dg-error "in .strub. context" } */
}
