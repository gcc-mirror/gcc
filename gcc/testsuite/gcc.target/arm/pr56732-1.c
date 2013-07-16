/* { dg-do compile } */
/* { dg-require-effective-target naked_functions } */
/* { dg-options "-O2 -Wall" } */
extern void bar();

void __attribute__((__naked__))
foo(void)
{
  bar ();
}

int __attribute__((naked))
zoo (int a, int b, int c, int d, int e, int f)
{
  bar ();
  return e;
}
/* Verify that __attribute__((naked)) produces a naked function that
   does not use bx to return. */
/* { dg-final { scan-assembler-not "\tbx\tlr" } } */
