/* Test HALT builtin.  */

void
test_halt (void)
{
  /* { dg-final { scan-assembler "halt" } } */
  __halt();
}

