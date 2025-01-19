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
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|pack|ret|sh1add|sh2add|sh3add|slli|srli|xori|or)" 5 } } */

unsigned long foo_0xcccccccccccccccd(void) { return 0xcccccccccccccccdUL; }
