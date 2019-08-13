/* { dg-do compile } */
/* { dg-options "-O" } */

void
f (void)
{
  register float s0 asm ("s0");
  register float s7 asm ("s7");
  register float s8 asm ("s8");
  register float s15 asm ("s15");
  register float s16 asm ("s16");
  register float s31 asm ("s31");
  asm volatile ("// s0 out: %s0" : "=w" (s0));
  asm volatile ("// s0 in: %s0" :: "x" (s0));
  asm volatile ("// s7 out: %s0" : "=w" (s7));
  asm volatile ("// s7 in: %s0" :: "x" (s7));
  asm volatile ("// s8 out: %s0" : "=w" (s8));
  asm volatile ("// s8 in: %s0" :: "x" (s8));
  asm volatile ("// s15 out: %s0" : "=w" (s15));
  asm volatile ("// s15 in: %s0" :: "x" (s15));
  asm volatile ("// s16 out: %s0" : "=w" (s16));
  asm volatile ("// s16 in: %s0" :: "x" (s16));
  asm volatile ("// s31 out: %s0" : "=w" (s31));
  asm volatile ("// s31 in: %s0" :: "x" (s31));
}

/* { dg-final { scan-assembler {\t// s0 out: s0\n.*[/]/ s0 in: s0\n} } } */
/* { dg-final { scan-assembler {\t// s7 out: s7\n.*[/]/ s7 in: s7\n} } } */
/* { dg-final { scan-assembler {\t// s8 out: s8\n.*[/]/ s8 in: s8\n} } } */
/* { dg-final { scan-assembler {\t// s15 out: s15\n.*[/]/ s15 in: s15\n} } } */
/* { dg-final { scan-assembler {\t// s16 out: s16\n.*\tfmov\t(s[0-7]), s16\n.*[/]/ s16 in: \1\n} } } */
/* { dg-final { scan-assembler {\t// s31 out: s31\n.*\tfmov\t(s[0-7]), s31\n.*[/]/ s31 in: \1\n} } } */
/* { dg-final { scan-assembler-not {\t// s16 in: s16\n} } } */
/* { dg-final { scan-assembler-not {\t// s31 in: s31\n} } } */
