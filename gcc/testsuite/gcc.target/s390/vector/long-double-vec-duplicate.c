/* { dg-do compile } */
/* { dg-options "-O3 -funroll-loops -march=z14 -mtune=z14" } */

long double a, b;
double *c;
long double *d;

void
e ()
{
  while (d != &a)
    *d++ = b * *c++;
}
