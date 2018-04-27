/* { dg-do run } */
/* { dg-options "-O3 -march=z900 --save-temps -mfunction-return-reg=thunk -mindirect-branch-table -fno-dwarf2-cfi-asm" } */

/* Make sure that we do not emit .cfi directives when -fno-dwarf2-cfi-asm is being used.  */

int
main ()
{
  return 0;
}

/* 1 x main
/* { dg-final { scan-assembler-times "jg\t__s390_indirect_jump" 1 } } */
/* { dg-final { scan-assembler "ex\t" } } */

/* { dg-final { scan-assembler-not "section\t.s390_indirect_jump" } } */
/* { dg-final { scan-assembler-not "section\t.s390_indirect_call" } } */
/* { dg-final { scan-assembler     "section\t.s390_return_reg" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_mem" } } */
