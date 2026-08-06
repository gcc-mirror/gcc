/* { dg-do compile } */
/* { dg-options "-Ofast -msse2" } */

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

/* { dg-final { scan-assembler-times {movabsq[ \t]+\$4575657222473777152,[ \t]+%rax} 1 { target { ! ia32 } } } } */
