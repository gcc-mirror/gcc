/* { dg-do compile } */

long double a;
double b;
void c (double, double);
void d (void)
{
  __asm__ ("" : "={fr2}" (b) : "{fr1}" (a));
  c (0, 0);
}
