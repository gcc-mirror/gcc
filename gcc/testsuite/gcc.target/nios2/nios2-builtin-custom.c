/* { dg-do compile } */
/* { dg-final { scan-assembler "custom" } } */

/* This test case used to cause an unrecognizable insn crash.  */

void foo (void)
{
  int offset = __builtin_custom_in(0x1);
}
