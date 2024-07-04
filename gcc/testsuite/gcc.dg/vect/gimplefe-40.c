/* { dg-do compile { target { int128 && vect_float } } } */
/* { dg-additional-options "-fgimple -Wno-psabi -w" } */
/* { dg-additional-options "-maltivec" { target { powerpc*-*-* && powerpc_altivec_ok } } } */

typedef float v4sf __attribute__((vector_size(16)));
v4sf __GIMPLE (ssa)
load (const void * p)
{
  __int128 unsigned _3;
  v4sf _4;
  v4sf _6;
  float _5;

  __BB(2):
  _3 = __MEM <__int128 unsigned, 8> ((char *)p_2(D));
  _4 = __VIEW_CONVERT <v4sf>(_3);
#if __SIZEOF_FLOAT__ == 4
  _5 = __BIT_FIELD_REF <float> (_4, 32, 64);
#else
  _5 = 1.0f;
#endif
  _6 = __BIT_INSERT (_4, _5, 0);
  return _6;
}
