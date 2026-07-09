/* { dg-do compile } */
/* { dg-options "-g" } */
/* { dg-final { scan-assembler-not "0x8000" } } */
/* { dg-final { scan-assembler ".dtpreld?word.*a\\s+.byte" } } */

__thread int a;
void
f(void)
{
  a += 1;
}
