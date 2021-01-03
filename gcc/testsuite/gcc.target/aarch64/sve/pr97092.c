/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O1 -ftree-vectorize -march=armv8.2-a+sve" } */

void g (void);
long a;

signed char
bar (int c, int d)
{
  return c + d;
}

void
foo (void)
{
  int f;
  for (; (long)foo < 4;) {
    f = 0;
    for (; f < 10; f++);
    g ();
    a = -4;
    for (; a; a = bar (a, 1));
  }
}
