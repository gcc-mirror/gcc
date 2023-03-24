/* PR tree-optimization/109274 */
/* { dg-do compile } */
/* { dg-options "-O2 " } */

float a, b, c;
int d;
float bar (void);

void
foo (void)
{
  a = 0 * -(2.0f * c);
  d = a != a ? 0 : bar ();
  b = c;
}

