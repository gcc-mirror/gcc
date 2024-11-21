/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* We aggressively skip as we really just need to test the basic synthesis
   which shouldn't vary based on the optimization level.  -O1 seems to work
   and eliminates the usual sources of extraneous dead code that would throw
   off the counts.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O2" "-O3" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gc_zba" } */

/* Rather than test for a specific synthesis of all these constants or
   having thousands of tests each testing one variant, we just test the
   total number of instructions.

   This isn't expected to change much and any change is worthy of a look.  */
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|pack|ret|sh1add|sh2add|sh3add|slli|srli|xori|or)" 54 } } */


unsigned long foo_0x7907d89a2857f2de(void) { return 0x7907d89a2857f2deUL; }
unsigned long foo_0x4fffaffb0fffefff(void) { return 0x4fffaffb0fffefffUL; }
unsigned long foo_0x23ff6fdc03ffeffc(void) { return 0x23ff6fdc03ffeffcUL; }
unsigned long foo_0x170faedc028ff6fc(void) { return 0x170faedc028ff6fcUL; }
unsigned long foo_0x5704dee01d019fa0(void) { return 0x5704dee01d019fa0UL; }
unsigned long foo_0x0589c731009d87e9(void) { return 0x0589c731009d87e9UL; }
unsigned long foo_0x0057857d00118119(void) { return 0x0057857d00118119UL; }
unsigned long foo_0x546b32e010e23d60(void) { return 0x546b32e010e23d60UL; }
unsigned long foo_0x64322a0021660e00(void) { return 0x64322a0021660e00UL; }
