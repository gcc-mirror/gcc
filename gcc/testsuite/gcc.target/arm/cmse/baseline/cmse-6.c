/* { dg-do compile } */
/* { dg-options "-mcmse" }  */

int __attribute__ ((cmse_nonsecure_call)) (*bar) (double);

int
foo (int a)
{
  return bar (2.0) + a + 1;
}

/* Remember dont clear r0 and r1, because we are passing the double parameter
 * for bar in them.  */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "movs\tr2, r4" } } */

/* Now we check that we use the correct intrinsic to call.  */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
