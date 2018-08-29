// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-options "-O2" }

inline double foo (double x)
{
  register double r;	// { dg-warning "ISO C\\+\\+17 does not allow 'register' storage class specifier" "" { target c++17 } }
  asm volatile ("fsqrt" : "=t" (r) : "0" (x));
  return r;
}

struct X
{
  ~X() {}
};

int b;

double bar (X x)
{
  if (b)
    return 1.0;
  return 36.0 * foo (36.0);
}

int main ()
{
  X x;
  if (bar (x) != 216.0)
    return 1;
  return 0;
}
