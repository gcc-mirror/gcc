/* { dg-do compile } */
/* { dg-options "-O2" } */

void foo (void);
void bar (void);

int
test (int a)
{
  int r;

  if (r = -a)
    foo ();
  else
    bar ();

  return r;
}

/* { dg-final { scan-assembler-not "testl" } } */
