/* PR target/69255 */
/* { dg-do compile } */
/* { dg-options "-msse4 -mno-avx" } */

#pragma GCC target "avx512vl"
#pragma GCC target ""
__attribute__ ((__vector_size__ (32))) long long a;
__attribute__ ((__vector_size__ (16))) int b;

void
foo (const long long *p, __attribute__ ((__vector_size__ (32))) long long *q)
{
  *q = __builtin_ia32_gather3siv4di (a, p, b, 1, 1);	/* { dg-error "needs isa option -m32 -mavx512vl" } */
}

/* { dg-warning "AVX vector return without AVX enabled changes the ABI" "" { target *-*-* } 13 } */
/* { dg-warning "AVX vector argument without AVX enabled changes the ABI" "" { target *-*-* } 13 } */
