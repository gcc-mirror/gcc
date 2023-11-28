/* { dg-do compile } */
/* { dg-options "-O0" } */
/* Check that naked functions don't place arguments on the stack at
   optimisation level '-O0'.  */
extern void bar (int);

void __attribute__((naked))
foo (int n, int m)
{
  bar (n + m);
}
/* { dg-final { scan-assembler "\tbl\\\s+@bar" } } */

/* Look for things that would appear in a non-naked function, but which
   should not appear in a naked function.  */
/* { dg-final { scan-assembler-not "\tj.*\\\s+\\\[blink\\\]" } } */
/* { dg-final { scan-assembler-not "\tst.*\\\s+" } } */
/* { dg-final { scan-assembler-not "\tmov\\\s+fp,sp" } } */
