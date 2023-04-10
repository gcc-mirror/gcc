/* PR target/108881 */
/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -mno-avx512vl" } */

typedef float __m256 __attribute__((__vector_size__(32)));
typedef __bf16 __v16bf __attribute__((__vector_size__(32)));
__v16bf a;
__m256 b, c;

void
foo (void)
{
  a = __builtin_ia32_cvtne2ps2bf16_v16bf (b, c);	/* { dg-warning "implicit declaration of function" } */
}							/* { dg-error "incompatible types when assigning to type" "" { target *-*-* } .-1 } */
