/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mtune=z14 -funroll-loops" } */

long double a;
int d;
void
b ()
{
  for (int c = 0; c < d; ++c)
    a = (a - c) / (c + 1);
}
