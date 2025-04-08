/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapxf" } */

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
