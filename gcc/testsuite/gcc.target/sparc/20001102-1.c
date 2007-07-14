/* { dg-do run } */
/* { dg-require-effective-target ultrasparc_hw } */
/* { dg-options "-O2 -mcpu=ultrasparc -mvis" } */

extern void abort (void);
extern void exit (int);

int foo(double a, int b, int c, double *d, int h)
{
  int f, g;
  double e;

l:
  f = (int) a;
  a -= (double) f;
  if (b == 1)
    {
      g = c;
      f += g;
      c -= g;
    }
  if (b == 2)
    {
      f++;
      h = c;
      goto l;
    }

  asm volatile ("" : : :
		"f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
		"f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",
		"f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",
		"f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31");

  return f & 7;
}

int main()
{
  if (foo(0.1, 1, 3, 0, 1) != 3)
    abort ();
  exit (0);
}
