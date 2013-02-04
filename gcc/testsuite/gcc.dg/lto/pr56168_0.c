/* { dg-lto-do run } */
/* { dg-lto-options { { -flto -O -ffast-math -fno-builtin } } } */

extern double pow(double, double);
extern void abort (void);
volatile double x = 1.0;
int main(int argc, char **argv)
{
  double d1 = x;
  double d2 = pow(d1, 1.0 / 3.0);
  double d3 = d1 * d1;
  if (d3 != 1.0 || d2 != 0.0)
    abort ();
  return 0;
}
