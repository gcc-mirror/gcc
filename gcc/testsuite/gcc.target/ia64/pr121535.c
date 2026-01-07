/* { dg-do compile } */
__attribute__ ((zero_call_used_regs("used"))) int f(int x)
{
  if (x == 1)
    return 0;
  return 1;
}
