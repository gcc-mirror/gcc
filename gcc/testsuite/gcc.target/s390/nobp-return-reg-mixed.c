/* { dg-do run } */
/* { dg-options "-O3 -march=z900 --save-temps -mfunction-return-reg=thunk -mindirect-branch-table" } */

/* We have to generate different thunks for indirect branches
   depending on whether the code is compiled for pre z10 machines or
   later.  This testcase makes sure this works within the same compile
   unit.  */

int __attribute__((noinline,noclone,target("arch=z10")))
bar (int a)
{
  return a + 2;
}

int __attribute__((noinline,noclone,target("arch=z9-ec")))
foo (int a)
{
  return a + 3;
}

int
main ()
{
  if (bar (42) != 44)
    __builtin_abort ();

  if (foo (42) != 45)
    __builtin_abort ();

  return 0;
}

/* 1 x bar, 1 x foo */
/* { dg-final { scan-assembler-times "jg\t__s390_indirect_jump" 2 } } */
/* 1 x foo */
/* { dg-final { scan-assembler-times "jg\t__s390_indirect_jump_r1use" 1 } } */

/* { dg-final { scan-assembler-times "ex\t" 1 } } */
/* { dg-final { scan-assembler-times "exrl\t" 1 } } */

/* { dg-final { scan-assembler-not "section\t.s390_indirect_jump" } } */
/* { dg-final { scan-assembler-not "section\t.s390_indirect_call" } } */
/* { dg-final { scan-assembler     "section\t.s390_return_reg" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_mem" } } */
