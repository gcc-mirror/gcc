/* { dg-do compile } */
/* { dg-options "-O0" } */
/* Check that naked functions don't place arguments on the stack at
   optimisation level '-O0'.  */

#if defined(__HS__) || defined(__EM__)
# define ILINK "ilink"
#else
# define ILINK "ilink1"
#endif

extern void bar (int);

void __attribute__((naked, interrupt(ILINK)))
foo (int n, int m)
{
  bar (n + m);
}
/* { dg-final { scan-assembler "\tbl\\\s+@bar" } } */

/* Look for things that would appear in a non-naked function, but which
   should not appear in a naked function.  */
/* { dg-final { scan-assembler-not "\trtie" } } */
/* { dg-final { scan-assembler-not "j.*\[ilink1\]" } } */
/* { dg-final { scan-assembler-not "\tst.*\\\s+" } } */
/* { dg-final { scan-assembler-not "\tmov\\\s+fp,sp" } } */
