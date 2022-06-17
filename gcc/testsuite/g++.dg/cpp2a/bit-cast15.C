// { dg-do compile }

struct S { short a, b; };
struct T { float a[4]; };
struct U { int b[4]; };

#if __SIZEOF_FLOAT__ == __SIZEOF_INT__
int
f1 (T &x)
{
  return __builtin_bit_cast (U, x).b[1];
}

float
f2 (int (&x)[4])
{
  return __builtin_bit_cast (T, x).a[2];
}
#endif
