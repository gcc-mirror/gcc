/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "memcmp" } } */
/* { dg-final { scan-assembler "1752394086" } } */

/* This should turn into four compare/jump pairs with -m32, within the
   limit of what the tuning considers acceptable for -O2.  */
int cmp (char *p, char *q)
{
  char *pa = __builtin_assume_aligned (p, 4);
  char *qa = __builtin_assume_aligned (q, 4);
  if (__builtin_memcmp (pa, qa, 16) != 0)
    return 1;
  return 0;
}
/* Since we have fast unaligned access, we should make a single
   constant comparison.  The constant becomes 1752394086.  */
int cmp2 (char *p)
{
  if (__builtin_memcmp (p, "fish", 4) != 0)
    return 1;
  return 0;
}
