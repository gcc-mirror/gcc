/* PR rtl-optimization/57131 */

extern void abort (void);

int
main ()
{
  volatile int x1 = 0;
  volatile long long x2 = 0;
  volatile int x3 = 0;
  volatile int x4 = 1;
  volatile int x5 = 1;
  volatile long long x6 = 1;
  long long t = ((x1 * (x2 << x3)) / (x4 * x5)) + x6;

  if (t != 1)
    abort ();
  return 0;
}
