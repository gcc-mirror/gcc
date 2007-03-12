/* Test for a bogus warning on comparison between signed and unsigned.
   Origin: Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 5/13/2001.  */

/* { dg-do compile } */
/* { dg-options "-Wsign-compare -fstrict-overflow" } */

extern void bar(void);

int foo(int x, int y, unsigned u)
{
  /* A COMPOUND_EXPR is non-negative if the last element is known to
     be non-negative.  */
  if (u < (bar(), -1)) /*{ dg-warning "signed and unsigned" "COMPOUND_EXPR" }*/
    return x;
  if (u < (bar(), 10))
    return x;
  if ((bar(), 10) < u)
    return x;
  if (u < (x ? (bar(),bar(),bar(),bar(),x==y) : 10))
    return x;
  if ((x ? 10 : (bar(),bar(),bar(),bar(),x==y)) < u)
    return x;

  /* Test an ABS_EXPR, which is by definition non-negative when
     -fstrict-overflow is used.  */
  if (u < __builtin_abs(x))
    return x;
  if (__builtin_abs(x) < u)
    return x;
  if (u < (x ? __builtin_abs(x) : 10))
    return x;
  if ((x ? 10: __builtin_abs(x)) < u)
    return x;

  /* A MODIFY_EXPR is non-negative if the new value is known to be
     non-negative.  */
  if (u < (x = -1)) /* { dg-warning "signed and unsigned" "MODIFY_EXPR" } */
    return x;
  if (u < (x = 10))
    return x;
  if ((x = 10) < u)
    return x;
  if (u < (x = (y ? (x==y) : 10)))
    return x;
  if ((x = (y ? 10 : (x==y))) < u)
    return x;

  return 0;
}
