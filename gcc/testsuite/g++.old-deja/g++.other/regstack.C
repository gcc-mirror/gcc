// Special g++ Options: -O2
// Skip if not target: i?86-*-*

inline double foo (double x)
{
  register double r;
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
