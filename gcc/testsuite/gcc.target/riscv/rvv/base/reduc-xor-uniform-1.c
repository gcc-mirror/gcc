/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -march=rv64im_zve64f -mabi=lp64 -fdump-tree-forwprop1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** reduc_xor_even:
**	li	a0,0
**	ret
*/
long __GIMPLE ()
reduc_xor_even (void)
{
  vbool32_t mask;
  vint64m2_t ones;
  long res;

  mask = _Literal (vint64m2_t) 0 == _Literal (vint64m2_t) 0;
  ones = .VEC_CONVERT (mask);
  res = .REDUC_XOR (ones);
  return res;
}

/*
** reduc_xor_unknown:
**	vsetvli	[a-x0-9]+,zero,e64,m1,ta,ma
**	...
**	vredxor\.vs	v[0-9]+,v[0-9]+,v[0-9]+
**	...
**	ret
*/
long __GIMPLE ()
reduc_xor_unknown (void)
{
  vbool64_t mask;
  vint64m1_t ones;
  long res;

  mask = _Literal (vint64m1_t) 0 == _Literal (vint64m1_t) 0;
  ones = .VEC_CONVERT (mask);
  res = .REDUC_XOR (ones);
  return res;
}

/* { dg-final { scan-tree-dump-times {\.REDUC_XOR} 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times {return 0;} 1 "forwprop1" } } */
