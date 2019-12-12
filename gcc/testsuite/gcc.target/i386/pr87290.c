/* PR middle-end/87290 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-times "and\[^\n\r]*-2147483633" 4 } } */
/* { dg-final { scan-assembler-times "\t\\\$13," 2 } } */
/* { dg-final { scan-assembler-times "\t\\\$-2147483645," 2 } } */

void f0 (void);

int
f1 (int x)
{
  return x % 16 == 13;
}

int
f2 (int x)
{
  return x % 16 == -13;
}

void
f3 (int x)
{
  if (x % 16 == 13)
    f0 ();
}

void
f4 (int x)
{
  if (x % 16 == -13)
    f0 ();
}
