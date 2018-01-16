/* { dg-do compile } */
/* { dg-options "-mavx512vbmi2 -mno-avx512f" } */

typedef long long __v8di __attribute__((vector_size (64)));
void
foo (__v8di *a, __v8di *b, __v8di *c, __v8di *d)
{
  *d = __builtin_ia32_vpshldv_v8di (*a, *b, *c); /* { dg-warning "implicit" } */
} /* { dg-error "incompatible types when assigning to type" "" { target *-*-* } .-1 } */
