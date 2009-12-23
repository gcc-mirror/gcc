/* { dg-do compile } */
/* { dg-options "-O2 -mieee" } */

double foo (void);
void bar (float, float);

void test (void)
{
  float f, g;

  f = foo();
  g = foo();
  asm ("");
  bar (f, g);
}
