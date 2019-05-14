/* { dg-do compile { target int128 } } */
/* { dg-options "-fgimple -Wno-psabi -w" } */

typedef float v4sf __attribute__((vector_size(16)));
v4sf __GIMPLE (ssa)
load (const void * p)
{
  __int128 unsigned _3;
  v4sf _4;

  __BB(2):
  _3 = __MEM <__int128 unsigned, 8> ((char *)p_2(D));
  _4 = __VIEW_CONVERT <v4sf>(_3);
  return _4;
}
