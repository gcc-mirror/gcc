/* { dg-do run } */
/* { dg-options "-O3" } */

extern void abort (void);
extern void exit (int);

int __attribute__((noinline, const))
ret4 (float value)
{
  return 4;
}

int __attribute__((noinline, const))
ret0 (float value)
{
  return 0;
}

float __attribute__((noinline))
test (float x, float y)
{
  int clsx = ret4 (x);
  int clsy = ret0 (y);

  if (clsx == 0 || clsy == 0
      || (y < 0 && clsx == 1 && clsy == 1))
    return x - y;

  return x < y ? 0 : x - y;
}

float a = 0.0, b;

int main (void)
{
  unsigned long e;
  b = a / a;
  __asm__ __volatile__ ("mov.m %0=ar.fpsr" : "=r" (e));
  e &= ~0x7e000UL;
  __asm__ __volatile__ ("mov.m ar.fpsr=%0" :: "r" (e) : "memory");
  a = test (0, b);
  __asm__ __volatile__ ("mov.m %0=ar.fpsr" : "=r" (e));
  if (e & 0x2000)
    abort ();
  exit (0);
}
