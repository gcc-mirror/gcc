/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-Wsign-compare" } */

int
f1 (unsigned char x)
{
  return (unsigned short) (~(unsigned short) x) == 0;		/* { dg-warning "promoted bitwise complement of an unsigned value is always nonzero" "" { target c } } */
}

int
f2 (unsigned char x)
{
  return (unsigned short) (~(unsigned short) x) == 5;		/* { dg-warning "comparison of promoted bitwise complement of an unsigned value with constant" "" { target c }  } */
}

int
f3 (unsigned char x)
{
  return (unsigned int) (~(unsigned short) x) == 0xffff0005U;	/* { dg-warning "comparison of promoted bitwise complement of an unsigned value with constant" } */
}

int
f4 (unsigned char x)
{
  return (unsigned int) (~(unsigned short) x) == 0xffff0005ULL;	/* { dg-warning "comparison of promoted bitwise complement of an unsigned value with constant" } */
}

int
f5 (unsigned char x)
{
  return (unsigned int) (~(unsigned short) x) == 0xffffff05U;	/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with constant" } */
}

int
f6 (unsigned char x)
{
  return (unsigned int) (~(unsigned short) x) == 0xffffff05ULL;	/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with constant" } */
}

int
f7 (unsigned char x)
{
  return (unsigned long long) (~(unsigned short) x) == 0xffffffffffffff05ULL;	/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with constant" } */
}

typedef unsigned short T;

int
f8 (T x)
{
  return (unsigned short) (~(unsigned short) x) == 0;		/* { dg-bogus "promoted bitwise complement of an unsigned value is always nonzero" } */
}

int
f9 (T x)
{
  return (unsigned short) (~(unsigned short) x) == 5;		/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with constant" } */
}

int
f10 (T x, unsigned char y)
{
  return (unsigned short) (~(unsigned short) x) == y;		/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with unsigned" } */
}

int
f11 (T x, unsigned char y)
{
  return (unsigned short) (~(unsigned short) x) == y;		/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with unsigned" } */
}

int
f12 (unsigned char x, unsigned char y)
{
  return (unsigned short) (~(unsigned short) x) == y;		/* { dg-warning "comparison of promoted bitwise complement of an unsigned value with unsigned" "" { target c } } */
}

int
f13 (unsigned char x, unsigned char y)
{
  return (unsigned short) (~(unsigned short) x) == y;		/* { dg-warning "comparison of promoted bitwise complement of an unsigned value with unsigned" "" { target c } } */
}

int
f14 (unsigned char x, unsigned int y)
{
  return (unsigned long long) (~x) == y;			/* { dg-warning "comparison of promoted bitwise complement of an unsigned value with unsigned" } */
}

int
f15 (unsigned short x, unsigned int y)
{
  return (long long) (~x) == y;					/* { dg-warning "comparison of promoted bitwise complement of an unsigned value with unsigned" } */
}

int
f16 (unsigned char x, unsigned short y)
{
  return (unsigned short) (~(unsigned short) x) == y;		/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with unsigned" } */
}

int
f17 (unsigned char x, unsigned short y)
{
  return (unsigned short) (~(unsigned short) x) == y;		/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with unsigned" } */
}

int
f18 (unsigned char x)
{
  return (unsigned int) (short) (~(unsigned short) x) == 0xffffff05ULL;	/* { dg-bogus "comparison of promoted bitwise complement of an unsigned value with constant" } */
}
