/* { dg-additional-options "-fgimple" } */

#include "tree-vect.h"

long g_73[2] = {6L,6L};
int __GIMPLE (ssa,startwith("loop")) __attribute__((noipa))
foo ()
{
  signed char g;
  int l;
  int _1;
  unsigned char _3;
  unsigned char _4;

  __BB(2):
  goto __BB3;

  __BB(3,loop_header(1)):
  l_5 = __PHI (__BB2: _Literal (int) -511973466, __BB3: 1);
  g_6 = __PHI (__BB2: _Literal (signed char) 0, __BB3: g_12);
  _1 = (int) g_6;
  g_73[_1] = 0l;
  _3 = (unsigned char) g_6;
  _4 = _3 + _Literal (unsigned char) 1;
  g_12 = (signed char) _4;
  if (g_12 > _Literal (signed char) 1)
    goto __BB4;
  else
    goto __BB3;

  __BB(4):
  l_14 = __PHI (__BB3: l_5);
  return l_14;
}

int main()
{
  check_vect ();
  if (foo () != 1)
    abort ();
  return 0;
}
