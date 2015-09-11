/* Check that the option -mcbranch-force-delay-slot works as expected on
   targets other than SH1, and that it compiles on SH1 targets without fuzz.  */
/* { dg-do compile }  */
/* { dg-options "-O2 -mcbranch-force-delay-slot" }  */
/* { dg-final { scan-assembler-times "nop" 2 { target { ! sh1 } } } }  */

int g (int, int);

int
f (int a, int b)
{
  /* Expected: 1x bt/s, 1x nop.  */
  if (a != 5)
    a = 10;

  /* Expected: 1x jmp, 1x nop.  */
  return g (a, b);
}
