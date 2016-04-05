/* PR target/70525 */
/* { dg-do assemble { target avx512bw } } */
/* { dg-options "-O2 -mavx512bw -mno-avx512vl" } */

typedef char v64qi __attribute__ ((vector_size (64)));
typedef short v32hi __attribute__ ((vector_size (64)));
typedef int v16si __attribute__ ((vector_size (64)));
typedef long long v8di __attribute__ ((vector_size (64)));

v64qi
f1 (v64qi x, v64qi y)
{
  return x & ~y;
}

v32hi
f2 (v32hi x, v32hi y)
{
  return x & ~y;
}

v16si
f3 (v16si x, v16si y)
{
  return x & ~y;
}

v8di
f4 (v8di x, v8di y)
{
  return x & ~y;
}
