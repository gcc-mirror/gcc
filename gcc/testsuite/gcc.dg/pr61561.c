/* PR c/61561.  */
/* { dg-do assemble } */
/* { dg-options " -w -O2" } */
/* { dg-require-effective-target alloca } */

int dummy (int a);

char a;
short b;

void mmm (void)
{
  char dyn[dummy (3)];
  a = (char)&dyn[0];
  b = (short)&dyn[0];
}
