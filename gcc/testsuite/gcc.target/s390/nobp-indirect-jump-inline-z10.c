/* { dg-do run } */
/* { dg-options "-O3 -march=z10 --save-temps -mindirect-branch-jump=thunk-inline -mindirect-branch-table" } */
/* { dg-require-effective-target label_values } */

/* This is a copy of the gcc.c-torture/execute/20040302-1.c
   testcase.  */

int code[]={0,0,0,0,1};

void
foo(int x) {
  volatile int b;
  b = 0xffffffff;
}

void
bar(int *pc) {
  static const void *l[] = {&&lab0, &&end};

  foo(0);
  goto *l[*pc];
 lab0:
  foo(0);
  pc++;
  goto *l[*pc];
 end:
  return;
}

int
main() {
  bar(code);
  return 0;
}

/* The two gotos in bar get merged.  */
/* { dg-final { scan-assembler-times "exrl" 1 } } */

/* { dg-final { scan-assembler-not "jg\t__s390_indirect_jump" } } */
/* { dg-final { scan-assembler     "section\t.s390_indirect_jump" } } */
/* { dg-final { scan-assembler-not "section\t.s390_indirect_call" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_reg" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_mem" } } */
