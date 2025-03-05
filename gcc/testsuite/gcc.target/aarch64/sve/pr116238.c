/* { dg-additional-options "-O2 -msve-vector-bits=128" } */

void foo();
typedef unsigned char v2qi __attribute__((vector_size(2)));
void f(v2qi *ptr)
{
  v2qi x = *ptr;
  asm volatile ("" :: "w" (x));
  asm volatile ("" ::: "d8", "d9", "d10", "d11", "d12", "d13", "d14", "d15");
  foo();
  asm volatile ("" :: "w" (x));
  *ptr = x;
}
