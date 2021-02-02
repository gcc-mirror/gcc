/* PR tree-optimization/94802 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "\ttestl\t" } } */
/* { dg-final { scan-assembler-times "\tcmpl\t" 8 } } */

void foo (void);

int
f1 (int a, int b)
{
  return (a - b) >= 0;
}

int
f2 (int a, int b)
{
  return (a - b) > 0;
}

int
f3 (int a, int b)
{
  return (a - b) <= 0;
}

int
f4 (int a, int b)
{
  return (a - b) < 0;
}

void
f5 (int a, int b)
{
  if ((a - b) >= 0)
    foo ();
}

void
f6 (int a, int b)
{
  if ((a - b) > 0)
    foo ();
}

void
f7 (int a, int b)
{
  if ((a - b) <= 0)
    foo ();
}

void
f8 (int a, int b)
{
  if ((a - b) < 0)
    foo ();
}
