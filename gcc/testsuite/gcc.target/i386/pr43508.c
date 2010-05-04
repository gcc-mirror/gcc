/* { dg-do compile } */
/* { dg-options "-g -O -msse3" } */

typedef float v4sf __attribute__ ((__vector_size__ (16)));
typedef int v4si __attribute__ ((__vector_size__ (16)));

v4sf bar(int);

v4sf foo(v4si vi)
{
  int x = __builtin_ia32_vec_ext_v4si (vi, 0);
  return bar(x);
}
