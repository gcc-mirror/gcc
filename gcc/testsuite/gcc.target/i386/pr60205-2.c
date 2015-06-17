/* PR target/60205 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f" } */
/* { dg-skip-if "no AVX512F vector" { *-*-mingw* } } */

typedef int __v16si __attribute__ ((__vector_size__ (64)));

extern __v16si x;

__v16si
foo (void)
{ /* { dg-warning "AVX512F vector return without AVX512F enabled changes the ABI" } */
  return x;
}
