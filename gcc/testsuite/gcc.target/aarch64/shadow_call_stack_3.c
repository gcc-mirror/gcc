/* Testing shadow call stack.  */
/* scs_push: str x30, [x18], #8 */
/* scs_pop: ldr x30, [x18, #-8]! */
/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=shadow-call-stack -ffixed-x18 -fno-exceptions" } */

int foo (int);

/* function not use x30.  */
int func1 (void)
{
  return 0;
}

/* function use x30.  */
int func2 (void)
{
  /* scs push */
  asm volatile ("":::"x30");

  return 0;
  /* scs pop */
}

/* sibcall.  */
int func3 (int a, int b)
{
  /* scs push */
  asm volatile ("":::"x30");

  return foo (a+b);
  /* scs pop */
}

/* eh_return.  */
int func4 (long offset, void *handler)
{
  /* Do not emit scs push/pop */
  asm volatile ("":::"x30");

  __builtin_eh_return (offset, handler);
}

/* { dg-final { scan-assembler-times {str\tx30, \[x18\], #?8} 2 } } */
/* { dg-final { scan-assembler-times {ldr\tx30, \[x18, #?-8\]!} 2 } } */
