/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-additional-options "-Wno-psabi" } */

typedef __attribute__((__vector_size__ (64))) unsigned char VC;
typedef __attribute__((__vector_size__ (64))) unsigned short VS;
typedef __attribute__((__vector_size__ (64))) unsigned int VI;
typedef __attribute__((__vector_size__ (64))) unsigned long long VL;
typedef __attribute__((__vector_size__ (64))) unsigned __int128 VV;

VC vc;
VS vs;
VI vi;
VL vl;

VV
foo (unsigned long long x, VV vv)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  x &= -((VC) vv)[0];
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  x &= -((VC) vv)[sizeof (__int128) - 1];
#else
  x &= -(unsigned char) (vv[0]);
#endif
  vi *= (VI) (VS){ -vs[0], vc[0], vs[1], vi[7], vs[7], vl[7], x, vi[5] };
  return x + vv;
}

int
main ()
{
  VV v = foo (0x01aabbccdd, (VV) { -0xff });
  if (v[0] != 0x01aabbccdd - 0xff)
    __builtin_abort ();
  if (v[1] != 0x01aabbccdd)
    __builtin_abort ();
  if (v[2] != 0x01aabbccdd)
    __builtin_abort ();
  if (v[3] != 0x01aabbccdd)
    __builtin_abort ();
}
