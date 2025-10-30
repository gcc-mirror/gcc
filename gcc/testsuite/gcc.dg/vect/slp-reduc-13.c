/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fgimple" } */

int q[2];

void __GIMPLE (ssa,guessed_local(16535624),startwith("loop"))
foo (int * r)
{
  int i;
  int sum2;
  int sum1;
  int _1;
  long unsigned int _2;
  long unsigned int _3;
  int * _4;
  int _24;
  __SIZETYPE__ _6;
  __SIZETYPE__ _7;
  int * _8;
  int _9;
  int _13;
  unsigned int _30;
  unsigned int _31;

  __BB(2,guessed_local(16535624)):
  goto __BB3(precise(134217728));

  __BB(3,loop_header(1),guessed_local(1057206200)):
  sum1_5 = __PHI (__BB5: sum1_18, __BB2: 0);
  sum2_26 = __PHI (__BB5: sum2_19, __BB2: 0);
  i_28 = __PHI (__BB5: i_20, __BB2: 0);
  _31 = __PHI (__BB5: _30, __BB2: 64u);
  _1 = i_28 * 2;
  _2 = (long unsigned int) _1;
  _3 = _2 * 4ul;
  _4 = r_17(D) + _3;
  _24 = __MEM <int> (_4);
  /* Deliberately have swapped operands here */
  sum1_18 = sum1_5 + _24;
  _13 = _1 + 1;
  _6 = (__SIZETYPE__) _13;
  _7 = _6 * 4ul;
  _8 = r_17(D) + _7;
  _9 = __MEM <int> (_8);
  /* versus here.  */
  sum2_19 = _9 + sum2_26;
  i_20 = i_28 + 1;
  _30 = _31 - 1u;
  if (_30 != 0u)
    goto __BB5(guessed(132118446));
  else
    goto __BB4(guessed(2099282));

  __BB(5,guessed_local(1040670576)):
  goto __BB3(precise(134217728));

  __BB(4,guessed_local(16535624)):
  sum1_33 = __PHI (__BB3: sum1_18);
  sum2_32 = __PHI (__BB3: sum2_19);
  q[0] = sum1_33;
  q[1] = sum2_32;
  return;
}

/* { dg-final { scan-tree-dump "SLP discovery of size 2 reduction group succeeded" "vect" } } */
