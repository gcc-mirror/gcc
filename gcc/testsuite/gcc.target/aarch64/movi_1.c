/* { dg-do compile } */
/* { dg-options "-O2" } */

void
dummy (short* b)
{
  /* { dg-final { scan-assembler "movi\tv\[0-9\]+\.4h, 0x4, lsl 8" } } */
  /* { dg-final { scan-assembler-not "movi\tv\[0-9\]+\.4h, 0x400" } } */
  /* { dg-final { scan-assembler-not "movi\tv\[0-9\]+\.4h, 1024" } } */
  register short x asm ("h8") = 1024;
  asm volatile ("" : : "w" (x));
  *b = x;
}
