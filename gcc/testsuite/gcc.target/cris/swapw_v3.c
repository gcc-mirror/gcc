/* Check that we don't use the swap insn by checking assembler output.
   The swap instruction was added in v8.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v3" } */
/* { dg-final { scan-assembler-not "\[ \t\]swapw\[ \t\]" } } */

unsigned int rot16_ior(unsigned int x) { return (x >> 16) | (x << 16); }
unsigned int rot16_xor(unsigned int x) { return (x >> 16) ^ (x << 16); }
unsigned int rot16_add(unsigned int x) { return (x >> 16) + (x << 16); }
