/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

typedef double v4si __attribute__((vector_size(32)));
typedef double v2si __attribute__((vector_size(16)));

void foo (v4si *dstp, v2si *srcp)
{
  v2si src = *srcp;
  *dstp = (v4si) { src[0], src[1], src[0], src[1] };
}

void bar (v4si *dstp, v2si *srcp)
{
  v2si src = *srcp;
  *dstp = (v4si) { src[0], src[0], src[0], src[0] };
}
