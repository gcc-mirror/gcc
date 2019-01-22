#pragma omp declare simd linear(p:1) linear(val(q):-1) linear(s:-3)
int
f1 (int *p, int *q, short *s)
{
  return *p + *q + *s;
}

/* { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } .-5 } */
/* { dg-final { scan-assembler-times "_ZGVbM4l4ln4ln6_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4l4ln4ln6_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4l4ln4ln6_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4l4ln4ln6_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8l4ln4ln6_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8l4ln4ln6_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM16l4ln4ln6_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN16l4ln4ln6_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */

#pragma omp declare simd linear(p:s) linear(q:t) uniform (s) linear(r:s) notinbranch simdlen(8) uniform(t)
int
f2 (int *p, short *q, int s, int r, int t)
{
  return *p + *q + r;
}

/* { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } .-5 } */
/* { dg-final { scan-assembler-times "_ZGVbN8ls2ls4uls2u_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN8ls2ls4uls2u_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8ls2ls4uls2u_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN8ls2ls4uls2u_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
