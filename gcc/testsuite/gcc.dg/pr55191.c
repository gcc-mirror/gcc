/* PR tree-optimization/55191 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int a, b;

void f(void)
{
  b = a || b;

  for(a = 0; a < 2; a++);
  while(1);
}

