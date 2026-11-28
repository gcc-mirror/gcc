/* PR middle-end/129083 */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fgimple -Wno-psabi -w" } */

/* A vector compare with a scalar result must not be sunk into the arms
   of a VEC_COND_EXPR: both arms are nonzero, but the selected value is
   all zeros for this input.  */

#include "tree-vect.h"

typedef int v4si __attribute__((vector_size (4 * sizeof (int))));
typedef v4si v4sib __attribute__((vector_mask));

int __GIMPLE (ssa) __attribute__((noipa))
f (v4si x)
{
  v4sib m;
  v4sib t;

__BB(2):
  m_2 = x_1(D) != _Literal (v4si) { 0, 0, 0, 0 };
  t_3 = m_2 ? _Literal (v4sib) { _Literal (int) -1, _Literal (int) -1, 0, _Literal (int) -1 } : _Literal (v4sib) { 0, 0, _Literal (int) -1, 0 };
  if (t_3 != _Literal (v4sib) { 0, 0, 0, 0 })
    goto __BB3;
  else
    goto __BB4;

__BB(3):
  return 0;

__BB(4):
  return 1;
}

int
main (void)
{
  check_vect ();
  v4si x = { 0, 0, 5, 0 };
  if (f (x) != 1)
    __builtin_abort ();
  return 0;
}
