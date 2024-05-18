/* { dg-options "-fgimple" } */
/* PR tree-optimization/115143 */
/* This used to ICE.
   minmax part of phiopt would transform,
   would transform `a!=0?min(a, b) : 0` into `min(a,b)`
   which was correct except b was defined by a phi in the inner
   bb which was not handled. */
unsigned __GIMPLE (ssa,startwith("phiopt"))
foo (unsigned a, unsigned b)
{
  unsigned j;
  unsigned _23;
  unsigned _12;

  __BB(2):
  if (a_6(D) != 0u)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  j_10 = __PHI (__BB2: b_11(D));
  _23 = __MIN (a_6(D), j_10);
  goto __BB4;

  __BB(4):
  _12 = __PHI (__BB3: _23, __BB2: 0u);
  return _12;

}
