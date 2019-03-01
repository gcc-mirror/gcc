/* { dg-do compile { target { powerpc*-*-* } } } */
/* powerpc_vsx_ok represents power7 */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power5" } */

/* Though the command line specifies power5 target, this function is
   to support power7.  */
__attribute__((target("cpu=power7")))
int
div_we (int a, int b)
{
  return __builtin_divwe (a, b);
}

/* { dg-final { scan-assembler-times "divwe "   1 } } */
