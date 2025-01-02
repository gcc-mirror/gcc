/* { dg-do compile } */
/* { dg-options "-mavx512f -O" } */

typedef __attribute__((__vector_size__ (64))) long long V;
unsigned long long x;

unsigned long long
foo()
{
  __builtin_ia32_pmovusqb512mem_mask (&x, (V){8000000000000000}, 255);
  return x;
}
