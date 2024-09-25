/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power5" } */

/* Though the command line specifies power5 target, this function is
   to support power6.  */
__attribute__((target("cpu=power6")))
double power6 (double a, double b)
{
  return __builtin_copysign (a, b);
}
/* { dg-final { scan-assembler-times "fcpsgn" 1 } } */
