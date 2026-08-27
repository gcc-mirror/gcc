/* Check that we use the swapnw insn by checking assembler output.
   The swap instruction was added in v8.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v8" } */
/* { dg-final { scan-assembler-times "\[ \t\]swapnw\[ \t\]" 2 } } */

unsigned int foo(unsigned int x)
{
  unsigned int t = ~x;
  t = (t >> 16) | (t << 16);
  return t;
}

unsigned int bar(unsigned int x)
{
  unsigned int t = x;
  t = (t >> 16) | (t << 16);
  return ~t;
}
