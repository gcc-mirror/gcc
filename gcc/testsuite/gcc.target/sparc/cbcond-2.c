/* { dg-do compile } */
/* { dg-options "-O -mcbcond" } */
/* { dg-require-effective-target lp64 } */

extern void foo (void);
extern void bar (void);

void cbcondne (long a)
{
  if (a != 0)
    foo ();
  bar ();
}

void cbconde (long a)
{
  if (a == 0)
    foo ();
  bar ();
}

void cbcondl (long a)
{
  if (a < 0)
    foo ();
  bar ();
}

void cbcondle (long a)
{
  if (a <= 0)
    foo ();
  bar ();
}

/* { dg-final { scan-assembler "cxbe\t%"  } } */
/* { dg-final { scan-assembler "cxbne\t%" } } */
/* { dg-final { scan-assembler "cxbl\t%"  } } */
/* { dg-final { scan-assembler "cxble\t%" } } */
