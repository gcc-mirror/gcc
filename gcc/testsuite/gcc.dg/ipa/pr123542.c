/* { dg-do compile } */
/* { dg-options "-O3 -std=gnu89" } */

double cos(double);
void sub(p1, p2, p3, p4, p5, p6) float p1, p2, p3, *p4, p5, p6;
{
  float ar2 = cos(*p4);
  if (p2)
    ar2 = 0.0;
  for (;;)
    *p4 += ar2;
}
void main() { sub(1.0, 2.0); }
