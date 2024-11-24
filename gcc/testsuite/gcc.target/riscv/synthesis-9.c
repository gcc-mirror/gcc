/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* We aggressively skip as we really just need to test the basic synthesis
   which shouldn't vary based on the optimization level.  -O1 seems to work
   and eliminates the usual sources of extraneous dead code that would throw
   off the counts.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O2" "-O3" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gc_zba_zbb_zbkb_zbs" } */

/* Rather than test for a specific synthesis of all these constants or
   having thousands of tests each testing one variant, we just test the
   total number of instructions.

   This isn't expected to change much and any change is worthy of a look.  */
/* { dg-final { scan-assembler-times "\\t(add|addi|bseti|li|pack|ret|sh1add|sh2add|sh3add|slli|srli|xori)" 40 } } */



unsigned long foo_0xf857f2def857f2de(void) { return 0xf857f2def857f2deUL; }
unsigned long foo_0x99660e6399660e63(void) { return 0x99660e6399660e63UL; }
unsigned long foo_0x937f1b75937f1b75(void) { return 0x937f1b75937f1b75UL; }
unsigned long foo_0xb5019fa0b5019fa0(void) { return 0xb5019fa0b5019fa0UL; }
unsigned long foo_0xb828e6c1b828e6c1(void) { return 0xb828e6c1b828e6c1UL; }
unsigned long foo_0x839d87e9839d87e9(void) { return 0x839d87e9839d87e9UL; }
unsigned long foo_0xc29617c1c29617c1(void) { return 0xc29617c1c29617c1UL; }
unsigned long foo_0xa4118119a4118119(void) { return 0xa4118119a4118119UL; }
unsigned long foo_0x8c01df7d8c01df7d(void) { return 0x8c01df7d8c01df7dUL; }
unsigned long foo_0xf0e23d6bf0e23d6b(void) { return 0xf0e23d6bf0e23d6bUL; }
