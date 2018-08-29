/* { dg-do run } */
/* { dg-options "-O3 -march=z10 -mzarch --save-temps -mindirect-branch-table" } */

int gl = 0;

int __attribute__((noinline,noclone))
bar (int a)
{
  return a + 2;
}

void __attribute__((function_return("thunk"),noinline,noclone))
foo (int a)
{
  int i;

  if (a == 42)
    return;

  for (i = 0; i < a; i++)
    gl += bar (i);
}

int
main ()
{
  foo (3);
  if (gl != 9)
    __builtin_abort ();

  return 0;
}

/* With -march=z10 -mzarch the shrink wrapped returns use compare and
   swap relative to jump to the exit block instead of making use of
   the conditional return pattern.
   FIXME: Use compare and branch register for that!!!! */

/* 2 x foo
/* { dg-final { scan-assembler-times "jg\t__s390_indirect_jump" 2 } } */
/* { dg-final { scan-assembler "exrl" } } */

/* { dg-final { scan-assembler-not "section\t.s390_indirect_jump" } } */
/* { dg-final { scan-assembler-not "section\t.s390_indirect_call" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_reg" } } */
/* { dg-final { scan-assembler     "section\t.s390_return_mem" } } */
