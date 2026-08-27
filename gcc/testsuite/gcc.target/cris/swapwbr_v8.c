/* Check that we use the swapwbr insn by checking assembler output.
   The swap instruction was added in v8.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v8" } */
/* { dg-final { scan-assembler "\[ \t\]swapwbr\[ \t\]" } } */

unsigned int foo(unsigned int x)
{
  return __builtin_bitreverse32(x);
}

