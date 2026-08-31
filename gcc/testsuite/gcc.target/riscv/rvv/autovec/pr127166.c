/* PR tree-optimization/127166 */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2 -fgimple" } */
/* { dg-additional-options "-fdump-tree-vect-details" } */

void __GIMPLE (ssa, startwith ("fix_loops"))
clip_signed_compare (unsigned short *out, const int *in)
{
  int i;
  int raw;
  int x;
  int neg;
  int shifted;
  int selected;
  unsigned short narrowed;
  _Bool cmp;
  const int *pin;
  const int *next_pin;
  unsigned short *pout;
  unsigned short *next_pout;

__BB(2):
  goto __BB3;

__BB(3):
  i_1 = __PHI (__BB2: 0, __BB4: i_2);
  pin_3 = __PHI (__BB2: in_4(D), __BB4: next_pin_5);
  pout_6 = __PHI (__BB2: out_7(D), __BB4: next_pout_8);
  raw_15 = __MEM <const int> (pin_3);
  x_9 = raw_15 | 1;
  cmp_10 = x_9 > 65535;
  neg_11 = -x_9;
  shifted_12 = neg_11 >> 31;
  selected_13 = cmp_10 ? shifted_12 : x_9;
  narrowed_14 = (unsigned short) selected_13;
  __MEM <unsigned short> (pout_6) = narrowed_14;
  next_pin_5 = pin_3 + 4ul;
  next_pout_8 = pout_6 + 2ul;
  i_2 = i_1 + 1;
  if (i_2 != 64)
    goto __BB4;
  else
    goto __BB5;

__BB(4):
  goto __BB3;

__BB(5):
  return;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "\\.SAT_TRUNC" "vect" } } */
