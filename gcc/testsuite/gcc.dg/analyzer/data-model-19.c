/* { dg-additional-options "-fgimple" } */

typedef long long int i64;

int __GIMPLE (ssa)
test (i64 * pA, i64 iB)
{
  __complex__ long long int D_37702;
  int D_37701;
  long long int _1;
  long long int _2;
  long long int _3;
  _Bool _4;
  __complex__ long long int _8;
  int _10;

  __BB(2):
  _1 = __MEM <i64> (pA_6(D));
  _8 = .ADD_OVERFLOW (_1, iB_7(D));
  _2 = __real _8;
  __MEM <i64> (pA_6(D)) = _2;
  _3 = __imag _8;
  _4 = (_Bool) _3;
  _10 = (int) _4;
  goto __BB3;

  __BB(3):
L0:
  return _10;

}
