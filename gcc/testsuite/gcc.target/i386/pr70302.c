/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -msse2" } */

long a, c, e;
int b, d;
unsigned long long f;

extern void fn2 (const char *, int, int, int);

void
fn1(long long p1)
{
  unsigned long long g;
  int i;
  for (; i;)
    if (e)
      g = c;
  if (a)
    f = p1;
  if (!f && !g)
    fn2("", b, d, d);
}
