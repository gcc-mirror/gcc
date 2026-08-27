/* Check that we use the swapw insn by checking assembler output.
   The swap instruction was added in v8.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v8" } */
/* { dg-final { scan-assembler-times "\[ \t\]swapw\[ \t\]" 3 } } */

unsigned int rot16_ior(unsigned int x) { return (x >> 16) | (x << 16); }
unsigned int rot16_xor(unsigned int x) { return (x >> 16) ^ (x << 16); }
unsigned int rot16_add(unsigned int x) { return (x >> 16) + (x << 16); }
