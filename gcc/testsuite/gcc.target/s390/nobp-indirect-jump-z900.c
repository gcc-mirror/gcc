/* { dg-do run } */
/* { dg-options "-O3 -march=z900 --save-temps -mindirect-branch-jump=thunk -mindirect-branch-table" } */
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

/* 2 x bar
/* { dg-final { scan-assembler-times "jg\t__s390_indirect_jump" 2 } } */
/* { dg-final { scan-assembler "ex\t" } } */

/* { dg-final { scan-assembler     "section\t.s390_indirect_jump" } } */
/* { dg-final { scan-assembler-not "section\t.s390_indirect_call" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_reg" } } */
/* { dg-final { scan-assembler-not "section\t.s390_return_mem" } } */
