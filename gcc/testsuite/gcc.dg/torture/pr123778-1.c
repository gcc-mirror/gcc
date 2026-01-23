/* { dg-do compile } */
/* { dg-options "-fgimple" } */

/* PR tree-optimization/123778 */

typedef char (*t)[3];
long __GIMPLE(ssa) f(char a, int d)
{
  _Bool _107;
  __PTRDIFF_TYPE__ _14;
  char *_36;

__BB(2):
  _107 = a_2(D) != _Literal (char) 0;
  _36 = _107
      ? _Literal (t)&__MEM <char[3]> ((void *)_Literal (t)&d + _Literal (void *) 2)
      : _Literal (t)&__MEM <char[3]> ((void *)_Literal (t)&d + _Literal (void *) 3);
  _14 = _36 - _Literal (char*)&d;
  return _14;
}
