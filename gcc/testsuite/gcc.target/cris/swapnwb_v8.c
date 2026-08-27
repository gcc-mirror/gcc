/* Check that we use the swapnwb insn by checking assembler output.
   The swap instruction was added in v8.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v8" } */
/* { dg-final { scan-assembler-times "\[ \t\]swapnwb\[ \t\]" 2 } } */

unsigned int foo(unsigned int x)
{
  return __builtin_bswap32(~x);
}

unsigned int bar(unsigned int x)
{
  return ~__builtin_bswap32(x);
}
