/* PR rtl-optimization/55838 */
/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops" } */

int a;
unsigned char c;

void
f (void)
{
  while (c++ < 2)
    c = a += 129;
}
