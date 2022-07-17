/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */
typedef unsigned __int128 uv1ti __attribute__ ((__vector_size__ (16)));
typedef unsigned long long uv2di __attribute__ ((__vector_size__ (16)));
typedef unsigned int uv4si __attribute__ ((__vector_size__ (16)));

uv1ti eq_v1ti(uv1ti x, uv1ti y) { return x == y; }
uv2di eq_v2di(uv2di x, uv2di y) { return x == y; }
uv4si eq_v4si(uv4si x, uv4si y) { return x == y; }

/* { dg-final { scan-assembler-times "pcmpeq" 3 } } */
/* { dg-final { scan-assembler "pshufd" } } */
