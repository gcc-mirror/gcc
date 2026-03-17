/* { dg-do compile } */
/* { dg-options "-Ofast -msse2 -fdump-rtl-x86_cse" } */

typedef __attribute__((__vector_size__(8))) float V;
V v;

void
foo()
{
  int i;
  float f;
  __builtin_memcpy(&f, &i, 1);
  v -= f;
  v /= f;
}

/* { dg-final { scan-rtl-dump {\(const_double:SF 0.0 \[0x0.0p\+0\]\)} "x86_cse" { target { ! ia32 } } } } */
