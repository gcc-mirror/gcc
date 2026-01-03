/* PR tree-optimization/123372 */
/* { dg-do compile } */
/* { dg-options "-O2 -fgimple" } */

#ifdef __SIZEOF_INT128__
#define T unsigned __int128
#else
#define T unsigned long long
#endif

unsigned int __GIMPLE (ssa,startwith("phiopt4"))
foo (T a, T b)
{
  T _4;
  unsigned int _1;
  bool _5;
  unsigned int _6;

  __BB(2):
  _5 = a_2(D) >= b_3(D);
  _4 = a_2(D) - b_3(D);
  _6 = (unsigned int) _4;
  _1 = _5 ? _6 : 0U;
  return _1;
}
