/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* We aggressively skip as we really just need to test the basic synthesis
   which shouldn't vary based on the optimization level.  -O1 seems to work
   and eliminates the usual sources of extraneous dead code that would throw
   off the counts.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O2" "-O3" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gc" } */

/* Rather than test for a specific synthesis of all these constants or
   having thousands of tests each testing one variant, we just test the
   total number of instructions.

   This isn't expected to change much and any change is worthy of a look.  */
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|pack|ret|sh1add|sh2add|sh3add|slli|srli|xori|or)" 45 } } */


unsigned long foo_0x7857f2de7857f2de(void) { return 0x7857f2de7857f2deUL; }
unsigned long foo_0x7fffdffe3fffefff(void) { return 0x7fffdffe3fffefffUL; }
unsigned long foo_0x1ffff7fe3fffeffc(void) { return 0x1ffff7fe3fffeffcUL; }
unsigned long foo_0x0a3fdbf0028ff6fc(void) { return 0x0a3fdbf0028ff6fcUL; }
unsigned long foo_0x014067e805019fa0(void) { return 0x014067e805019fa0UL; }
unsigned long foo_0x09d87e90009d87e9(void) { return 0x09d87e90009d87e9UL; }
unsigned long foo_0x2302320000118119(void) { return 0x2302320000118119UL; }
unsigned long foo_0x000711eb00e23d60(void) { return 0x000711eb00e23d60UL; }
unsigned long foo_0x5983800001660e00(void) { return 0x5983800001660e00UL; }
