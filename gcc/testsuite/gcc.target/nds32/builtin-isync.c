/* Verify that we generate isync instruction with builtin function.  */

/* { dg-do compile }  */
/* { dg-options "-O0" }  */
/* { dg-final { scan-assembler "\\tisync" } }  */

void
test (void)
{
  int *addr = (int *) 0x53000000;
  __builtin_nds32_isync (addr);
}
