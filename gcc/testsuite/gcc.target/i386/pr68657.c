/* PR c/68657 */
/* { dg-options "-mno-avx512f -Werror=psabi" } */

typedef int V __attribute__((vector_size (64)));

void foo (V x, V *y) {	/* { dg-error "AVX512F vector argument without AVX512F enabled" } */
  *y = x;
}

V bar (V *x) {		/* { dg-error "AVX512F vector return without AVX512F enabled" } */
  return *x;
}

/* { dg-message "The ABI for passing parameters with 64-byte alignment has changed" "" { target *-*-* } 6 } */
/* { dg-message "some warnings being treated as errors" "" { target *-*-* } 0 } */
