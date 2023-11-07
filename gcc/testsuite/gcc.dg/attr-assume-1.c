/* Portable assumptions */
/* { dg-do run } */
/* { dg-options "-std=c23" } */

int
f1 (int i)
{
  [[gnu::assume (i == 42)]];
  return i;
}

int
f2 (int i)
{
  __attribute__ ((assume (++i == 44)));
  return i;
}

int a;
int *volatile c;

int
f3 ()
{
  ++a;
  return 1;
}

int
f4 (double x)
{
  [[gnu::assume (__builtin_isfinite (x) && x >= 0.0)]];
  return __builtin_isfinite (__builtin_sqrt (x));
}

double
f5 (double x)
{
  __attribute__((assume (__builtin_isfinite (__builtin_sqrt (x)))));
  return __builtin_sqrt (x);
}

int
f6 (int x)
{
  [[gnu::assume (x == 93 ? 1 : 0)]];
  return x;
}

int
main ()
{
  int b = 42;
  double d = 42.0, e = 43.0;
  c = &b;
  [[__gnu__::__assume__ (f3 ())]];
  if (a)
    __builtin_abort ();
  [[gnu::assume (++b == 43)]];
  if (b != 42 || *c != 42)
    __builtin_abort ();
  __attribute__((assume (d < e)));
  int i = 90, j = 91, k = 92;
  [[gnu::__assume__ (i == 90), gnu::assume (j <= 91)]] [[gnu::assume (k >= 92)]]
  ;
  __attribute__((__assume__ (i == 90), assume (j <= 91))) __attribute__((assume (k >= 92)));
  if (f6 (93) != 93)
    __builtin_abort ();
}
