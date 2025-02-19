/* { dg-do compile } */
/* { dg-options "-O0 -march=x86-64-v3 -mavx10.2-256 -mno-avxvnniint16" } */
typedef int v8si __attribute__ ((vector_size (32)));
v8si
foo (v8si a, v8si b, v8si c)
{
  return __builtin_ia32_vpdpwsud256 (a, b, c);
}
