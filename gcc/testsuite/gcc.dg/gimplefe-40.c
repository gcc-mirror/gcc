/* { dg-do compile { target int128 } } */
/* { dg-options "-fgimple" } */

typedef float v4sf __attribute__((vector_size(16)));
float __GIMPLE (ssa)
load (const void * p)
{
  __int128 unsigned _3;
  v4sf _4;
  float _5;

  __BB(2):
  _3 = __MEM <__int128 unsigned, 8> ((char *)p_2(D));
  _4 = __VIEW_CONVERT <v4sf>(_3);
#if __SIZEOF_FLOAT__ == 4
  _5 = __BIT_FIELD_REF <float> (_4, 32, 64);
#else
  _5 = 1.0f;
#endif
  return _5;
}
