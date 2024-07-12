/* PR target/97367 */
/* { dg-options "-mdejagnu-cpu=G5" } */

/* Verify we emit a ".machine power4" and ".machine altivec" rather
   than a ".machine power7".  */

int dummy (void)
{
  return 0;
}

/* { dg-final { scan-assembler {\.machine power4\M} } } */
/* { dg-final { scan-assembler {\.machine altivec\M} } } */
