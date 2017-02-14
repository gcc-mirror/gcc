/* { dg-do compile } */
/* { dg-options "-O -mcbcond" } */

extern void foo (void);
extern void bar (void);

void cbcondne (int a)
{
  if (a != 0)
    foo ();
  bar ();
}

void cbconde (int a)
{
  if (a == 0)
    foo ();
  bar ();
}

void cbcondl (int a)
{
  if (a < 0)
    foo ();
  bar ();
}

void cbcondle (int a)
{
  if (a <= 0)
    foo ();
  bar ();
}

/* { dg-final { scan-assembler "cwbe\t%"  { target ilp32 } } } */
/* { dg-final { scan-assembler "cwbne\t%" { target ilp32 } } } */
/* { dg-final { scan-assembler "cwbl\t%"  } } */
/* { dg-final { scan-assembler "cwble\t%" } } */
