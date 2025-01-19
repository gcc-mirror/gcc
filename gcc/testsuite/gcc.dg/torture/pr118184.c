/* { dg-do run { target { float128 && lp64 } } } */

union u1
{
  _Float128 ld;
  unsigned long l[2];
};

[[gnu::noipa]]
unsigned long m()
{
  return 1000;
}

[[gnu::noinline]]
_Float128 f(void)
{
  union u1 u;
  u.ld = __builtin_nanf128("");
  u.l[0] = m();
  return u.ld;
}

int main()
{
   union u1 u;
   u.ld = f();
   union u1 u2;
   u2.ld = __builtin_nanf128("");
   u2.l[0] = m();
   if (u.l[0] != u2.l[0])
     __builtin_abort();
   if (u.l[1] != u2.l[1])
     __builtin_abort();
   return 0;
}
