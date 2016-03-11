/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -msse2 -Wno-uninitialized -Wno-maybe-uninitialized" } */

long long a;
void fn1();
void fn2(unsigned t, int a_int, unsigned p)
{
  long long x;
  int i, j = 1;
  t = i;
  for (; j;) {
    a = x;
    x = 1 + t;
    j += a_int;
    fn1();
    if (x == 1)
      return;
  }
}
