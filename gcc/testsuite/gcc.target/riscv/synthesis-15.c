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
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|pack|ret|sh1add|sh2add|sh3add|slli|srli|xori|or)" 60 } } */

unsigned long foo_0xcafe605f35019fa0(void) { return 0xcafe605f35019fa0UL; }
unsigned long foo_0x87a80d217857f2de(void) { return 0x87a80d217857f2deUL; }
unsigned long foo_0x6699f19c19660e63(void) { return 0xe699f19c19660e63UL; }
unsigned long foo_0xec80e48a137f1b75(void) { return 0xec80e48a137f1b75UL; }
unsigned long foo_0xc7d7193e3828e6c1(void) { return 0xc7d7193e3828e6c1UL; }
unsigned long foo_0xfc627816039d87e9(void) { return 0xfc627816039d87e9UL; }
unsigned long foo_0xbd69e83e429617c1(void) { return 0xbd69e83e429617c1UL; }
unsigned long foo_0xdbee7ee624118119(void) { return 0xdbee7ee624118119UL; }
unsigned long foo_0xf3fe20820c01df7d(void) { return 0xf3fe20820c01df7dUL; }
unsigned long foo_0x8f1dc29470e23d6b(void) { return 0x8f1dc29470e23d6bUL; }
