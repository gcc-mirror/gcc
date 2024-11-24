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
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|pack|ret|sh1add|sh2add|sh3add|slli|srli|xori|or)" 65 } } */


unsigned long foo_0x7857faae7857f2de(void) { return 0x7857faae7857f2deUL; }
unsigned long foo_0x0ffff7fe0fffefff(void) { return 0x0ffff7fe0fffefffUL; }
unsigned long foo_0x7857f2de7857faae(void) { return 0x7857f2de7857faaeUL; }
unsigned long foo_0x7857f2af7857faae(void) { return 0x7857f2af7857faaeUL; }
unsigned long foo_0x5fbfffff5fbffae5(void) { return 0x5fbfffff5fbffae5UL; }
unsigned long foo_0x3d3079db3d3079ac(void) { return 0x3d3079db3d3079acUL; }
unsigned long foo_0x046075fe046078a8(void) { return 0x046075fe046078a8UL; }
unsigned long foo_0x2411811a24118119(void) { return 0x2411811a24118119UL; }
unsigned long foo_0x70e23d6a70e23d6b(void) { return 0x70e23d6a70e23d6bUL; }
unsigned long foo_0x0c01df8c0c01df7d(void) { return 0x0c01df8c0c01df7dUL; }
unsigned long foo_0x7fff07d07fff0000(void) { return 0x7fff07d07fff0000UL; }
