#pragma omp declare simd linear(p:1) linear(q:-1) linear(s:-3)
int
f1 (int *p, int *q, short *s)
{
  return *p + *q + *s;
}

// { dg-final { scan-assembler-times "_ZGVbM4l4ln4ln6__Z2f1PiS_Ps:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4l4ln4ln6__Z2f1PiS_Ps:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4l4ln4ln6__Z2f1PiS_Ps:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4l4ln4ln6__Z2f1PiS_Ps:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8l4ln4ln6__Z2f1PiS_Ps:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8l4ln4ln6__Z2f1PiS_Ps:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16l4ln4ln6__Z2f1PiS_Ps:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16l4ln4ln6__Z2f1PiS_Ps:" 1 { target { i?86-*-* x86_64-*-* } } } }

#pragma omp declare simd linear(p:s) linear(q:t) uniform (s) linear(r:s) notinbranch simdlen(8) uniform(t)
int
f2 (int *p, short *q, int s, int r, int &t)
{
  return *p + *q + r;
}

// { dg-final { scan-assembler-times "_ZGVbN8ls2ls4uls2u__Z2f2PiPsiiRi:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN8ls2ls4uls2u__Z2f2PiPsiiRi:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8ls2ls4uls2u__Z2f2PiPsiiRi:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN8ls2ls4uls2u__Z2f2PiPsiiRi:" 1 { target { i?86-*-* x86_64-*-* } } } }

#pragma omp declare simd linear(ref(p):s) linear(val(q):t) uniform (s) linear(uval(r):s) notinbranch simdlen(8) uniform(t)
int
f3 (int &p, short &q, int s, int &r, int &t)
{
  return p + q + r;
}

// { dg-final { scan-assembler-times "_ZGVbN8Rs2Ls4uUs2u__Z2f3RiRsiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN8Rs2Ls4uUs2u__Z2f3RiRsiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8Rs2Ls4uUs2u__Z2f3RiRsiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN8Rs2Ls4uUs2u__Z2f3RiRsiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
