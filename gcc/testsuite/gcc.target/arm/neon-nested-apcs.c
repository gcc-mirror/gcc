/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-fno-omit-frame-pointer -mapcs-frame -O" }
/* { dg-add-options arm_neon } */

extern void abort (void);

float data;

void __attribute__((noinline, noclone)) bar (float f)
{
  data = f;
}

float __attribute__((noinline, noclone)) foo (float f)
{
  int error_reported = 0;

  void __attribute__((noinline, noclone)) 
  nested (int a, int b, int c, int d, float f0, float f1, float f2, float f3)
  {
    float e;

    if (f3 > f2)
      e = f3;
    else
      e = f2;

    if (f0 - f1 > e)
      {
	error_reported = a + b + c + d;
	bar (f0);
	bar (e);
      }
  }

  nested (1, 2, 3, 4, 1.0, 1.0, 3.5, 4.2);
  return f + (float)error_reported;
}

#define PI 3.1415927f

int main (void)
{
  if (foo (PI) != PI)
    abort ();
  return 0;
}
