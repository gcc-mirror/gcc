/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-final { scan-assembler     "ashq .*,\\\$0xffffffffffffffff," } } */
/* { dg-final { scan-assembler-not "ashq .*,\\\$-1," } } */

unsigned long long
a (unsigned long i)
{
  return ~(unsigned long long) 0 << i;
}
