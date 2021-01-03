/* { dg-do compile } */
/* { dg-options "-O0 -mno-avxvnni -mavx512vnni -mavx512vl" } */
typedef int v8si __attribute__ ((vector_size (32)));
v8si
foo (v8si a, v8si b, v8si c)
{
  return __builtin_ia32_vpdpbusd_v8si (a, b, c);
}
