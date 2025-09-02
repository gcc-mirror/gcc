/* { dg-additional-options "-fgimple -fno-tree-scev-cprop" } */

#include "tree-vect.h"

int a, b, f, j, i, *m, n, o, p = 1, q;

int __GIMPLE (ssa,guessed_local(1073741824),startwith("loop"))
main ()
{
  int D_3005;
  int D_3003;
  int d;
  int * e;
  long unsigned int _9;
  long unsigned int _10;
  int * _11;
  int _15;
  int _32;

  __BB(2,guessed_local(1073741824)):
  check_vect ();
  e_6 = __builtin_malloc (64ul);
  goto __BB3(precise(134217728));

  __BB(3,loop_header(2),guessed_local(8687547538)):
  d_29 = __PHI (__BB3: d_8, __BB2: 0);
  d_8 = d_29 + 1;
  _9 = (long unsigned int) d_29;
  _10 = _9 * 4ul;
  _11 = e_6 + _10;
  __MEM <int> (_11) = d_29;
  if (d_8 <= 15)
    goto __BB3(guessed(119453778));
  else
    goto __BB4(guessed(14763950));

  __BB(4,guessed_local(955630224)):
  if (d_8 != 16)
    goto __BB9(guessed(58814510));
  else
    goto __BB5(guessed(75403218));

  __BB(5,guessed_local(536870912)):
  a = 0;
  if (d_8 > 0)
    goto __BB6(guessed(119453778));
  else
    goto __BB8(guessed(14763950));

  __BB(6,loop_header(1),guessed_local(4343773769)):
  _32 = __PHI (__BB6: _15, __BB5: 0);
  _15 = _32 + 1;
  if (d_29 > _32)
    goto __BB6(guessed(119453778));
  else
    goto __BB7(guessed(14763950));

  __BB(7,guessed_local(477815112)):
  a = _15;
  goto __BB8(precise(134217728));

  __BB(8,guessed_local(1073741824)):
  __builtin_free (e_6);
  f = 0;
  return 0;

  __BB(9,precise(0)):
  a = d_8;
  f = 1;
  __builtin_abort ();

}


