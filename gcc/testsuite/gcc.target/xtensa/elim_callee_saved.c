/* { dg-do compile } */
/* { dg-options "-O2 -mabi=call0" } */

extern void foo(void);

/* eliminated one register (the reservoir of variable 'a') by its stack slot through the stack pointer.  */
int test0(int a) {
  int array[252];  /* the maximum bound of non-large stack.  */
  foo();
  asm volatile("" : : "m"(array));
  return a;
}

/* cannot eliminate if large stack is needed, because the offset from TOS cannot fit into single L32I/S32I instruction.  */
int test1(int a) {
  int array[10000];  /* requires large stack.  */
  foo();
  asm volatile("" : : "m"(array));
  return a;
}

/* register A15 is the reservoir of the stack pointer and cannot be eliminated if the frame pointer is needed.
   other registers still can be, but through the frame pointer rather the stack pointer.  */
int test2(int a) {
  int* p = __builtin_alloca(16);
  foo();
  asm volatile("" : : "r"(p));
  return a;
}

/* in -O0 the composite hard registers may still remain unsplitted at pro_and_epilogue and must be excluded.  */
extern double bar(void);
int __attribute__((optimize(0))) test3(int a) {
  return bar() + a;
}

/* { dg-final { scan-assembler-times "mov\t|mov.n\t" 21 } } */
/* { dg-final { scan-assembler-times "a15, 8" 2 } } */
