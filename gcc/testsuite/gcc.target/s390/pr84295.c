/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -fgnu89-inline --save-temps -mfunction-return-reg=thunk -mindirect-branch-table" } */

extern void foo (void);
extern __inline  void foo (void) {}
void foo (void) {}

/* { dg-final { scan-assembler-times "jg\t__s390_indirect_jump" 1 } } */
/* { dg-final { scan-assembler "ex\t" } } */

/* { dg-final { scan-assembler-not "section\t.s390_indirect_jump" } } */
/* { dg-final { scan-assembler-not "section\t.s390_indirect_call" } } */
/* { dg-final { scan-assembler     "section\t.s390_return_reg" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_mem" } } */
