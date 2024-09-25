/* { dg-do compile } */
/* { dg-additional-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#pragma GCC target "+nosve"

#define vect8 __attribute__((vector_size(8) ))

/**
**bar1:
**	fcmgt	v([0-9]+).2s, v[0-9]+.2s, v[0-9]+.2s
**	bic	v0.8b, v2.8b, v\1.8b
**	ret
*/
extern "C"
vect8 int bar1(vect8 float a, vect8 float b, vect8 int c)
{
  return (a > b) ? 0 : c;
}

/**
**bar2:
**	fcmgt	v([0-9]+).2s, v[0-9]+.2s, v[0-9]+.2s
**	orn	v0.8b, v2.8b, v\1.8b
**	ret
*/
extern "C"
vect8 int bar2(vect8 float a, vect8 float b, vect8 int c)
{
  return (a > b) ? c : -1;
}

// We should produce a BIT_ANDC and BIT_IORC here.

// { dg-final { scan-tree-dump ".BIT_ANDN " "optimized" } }
// { dg-final { scan-tree-dump ".BIT_IORN " "optimized" } }

