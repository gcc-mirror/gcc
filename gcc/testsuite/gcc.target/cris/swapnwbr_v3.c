/* Check that we don't use the swap insn by checking assembler output.
   The swap instruction was added in v8.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v3" } */
/* { dg-final { scan-assembler-not "\[ \t\]swapnwbr\[ \t\]" } } */

unsigned int foo(unsigned int x)
{
  return __builtin_bitreverse32(~x);
}

unsigned int bar(unsigned int x)
{
  return ~__builtin_bitreverse32(x);
}
