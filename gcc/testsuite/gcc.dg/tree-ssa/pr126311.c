/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* gcd(6, 10) == 2: (t * 6) / 10 -> (t * 3) / 5  */
int src_pos (int t) {
  return (t * 6) / 10;
}

/* Same with a negative multiplier: (t * -6) / 10 -> (t * -3) / 5  */
int src_neg (int t) {
  return (t * -6) / 10;
}

/* Reduced: b is unsigned short, so t is in [0, 65535] and t * 6u cannot wrap.  */
unsigned int src_unsigned_bounded (unsigned short b) {
  unsigned int t = b;
  return (t * 6u) / 10u;
}

/* Not reduced: t may be any unsigned value, so ranger can't prove 
   t * 6u doesn't wrap.  */
unsigned int src_unsigned_unbounded (unsigned int t) {
  return (t * 6u) / 10u;
}

/* No common factor: gcd(7, 10) == 1 (should stay / 10)  */
int src_gcd_one (int t) {
  return (t * 7) / 10;
}

/* Not reduced: the product has another use (mult:s).  */
int src_multi_use (int t, int *p) {
  int m = t * 6;
  *p = m;
  return m / 10;
}

/* { dg-final { scan-tree-dump-times " / 5" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " / 10" 3 "optimized" } } */
