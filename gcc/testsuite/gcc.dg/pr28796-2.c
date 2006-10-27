/* { dg-do run } */
/* { dg-options "-O2 -funsafe-math-optimizations -fno-finite-math-only" } */

extern void abort (void);

void foo(float f)
{
  if (__builtin_isunordered (f, f) != 1)
    abort ();
  if (__builtin_isnan (f) != 1)
    abort ();
  if (__builtin_finite (f) != 0)
    abort ();
}

int main()
{
  float f = __builtin_nanf("");
  foo(f);
  return 0;
}
