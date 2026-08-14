/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-widening_mul" } */

/* convert_plusminus_to_widen must not build a WIDEN_MULT_PLUS_EXPR when the
   multiply reaches the addition through a conversion whose result has more
   than one use.  The multiply is then duplicated into every addition instead
   of being replaced by one, and the original stays live.  Here that turns one
   shift and three adds into three widening multiply-accumulates plus the
   constants they need in registers.  */

void
f (const signed char *base, int i, const signed char **out)
{
  unsigned long o = (unsigned long) ((long) i * 128);
  out[0] = base + o;
  out[1] = base + o + 16;
  out[2] = base + o + 32;
  out[3] = base + o + 48;
}

/* { dg-final { scan-tree-dump-not "WIDEN_MULT_PLUS_EXPR" "widening_mul" } } */
