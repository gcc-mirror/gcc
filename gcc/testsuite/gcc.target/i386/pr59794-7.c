/* PR target/59794 */
/* { dg-options "-O2 -mno-avx" } */
/* { dg-skip-if "no AVX vector" { *-*-mingw* } } */

typedef int __v8si __attribute__ ((__vector_size__ (32)));

extern __v8si x;

__v8si
foo (void)
{ /* { dg-warning "AVX vector return without AVX enabled changes the ABI" } */
  return x;
}
