// { dg-do compile }

struct S { short a, b; };
struct T { float a[16]; };
struct U { int b[16]; };

#if __SIZEOF_FLOAT__ == __SIZEOF_INT__
int
f1 (float x)
{
  return __builtin_bit_cast (int, x);
}
#endif

#if 2 * __SIZEOF_SHORT__ == __SIZEOF_INT__
S
f2 (int x)
{
  return __builtin_bit_cast (S, x);
}

int
f3 (S x)
{
  return __builtin_bit_cast (int, x);
}
#endif

#if __SIZEOF_FLOAT__ == __SIZEOF_INT__
U
f4 (T &x)
{
  return __builtin_bit_cast (U, x);
}

T
f5 (int (&x)[16])
{
  return __builtin_bit_cast (T, x);
}
#endif

int
f6 ()
{
  return __builtin_bit_cast (unsigned char, (signed char) 0);
}
