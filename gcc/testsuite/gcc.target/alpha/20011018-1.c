/* { dg-do compile } */
/* { dg-options "-mieee" } */

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
