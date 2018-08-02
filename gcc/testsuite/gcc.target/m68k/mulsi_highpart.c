/* { dg-do compile } */
/* { dg-options "-O2 -m68020" } */
/* Don't optimize away a compare after [us]mulsi_highpart.  */
/* { dg-final { scan-assembler {tst\.?l} } } */
int cmp (unsigned int a, unsigned int b)
{
  return (a > 0xffffffff / b);
}
