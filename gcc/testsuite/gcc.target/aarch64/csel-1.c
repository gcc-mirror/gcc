/* { dg-do compile } */
/* { dg-options "-O2" } */

/* These 2 functions should be the same; even though there is a label in f1. 
   The label should not make a difference in code generation.
   There sign extend should be removed as it is not needed. */
void f(int t, int a, short *b)
{
  short t1 = 1;
  if (a)
    {
      t1 = t;
    }
  *b = t1;
}

void f1(int t, int a, short *b)
{
  short t1 = 1;
  if (a)
    {
      label1: __attribute__((unused))
      t1 = t;
    }
  *b = t1;
}

/* { dg-final { scan-assembler-not "sxth\t" } } */
