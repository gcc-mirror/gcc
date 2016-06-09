/* { dg-do compile } */

int
f1 (void)
{
  int x = __builtin_add_overflow ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_add_overflow_p ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_sub_overflow_p ();	/* { dg-error "too few arguments to function" } */
  x += __builtin_mul_overflow_p ();	/* { dg-error "too few arguments to function" } */
  return x;
}

int
f2 (int a, int b, int *c, int d)
{
  int x = __builtin_add_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_sub_overflow (a, b, c, d, d, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_mul_overflow (a, b, c, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_add_overflow_p (a, b, d, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_sub_overflow_p (a, b, d, d, 1, d);	/* { dg-error "too many arguments to function" } */
  x += __builtin_mul_overflow_p (a, b, d, d);	/* { dg-error "too many arguments to function" } */

  return x;
}

enum E { e0 = 0, e1 = 1 };

#ifndef __cplusplus
#define bool _Bool
#endif

int
f3 (float fa, int a, _Complex long int ca, double fb, void *pb, int b, enum E eb, bool bb, int *c)
{
  int x = __builtin_add_overflow (fa, b, c);	/* { dg-error "argument 1 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow (ca, b, c);	/* { dg-error "argument 1 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_mul_overflow (a, fb, c);	/* { dg-error "argument 2 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_add_overflow (a, pb, c);	/* { dg-error "argument 2 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow (a, eb, c);
  x += __builtin_mul_overflow (a, bb, c);
  x += __builtin_add_overflow_p (fa, b, a);	/* { dg-error "argument 1 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow_p (ca, b, eb);	/* { dg-error "argument 1 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_mul_overflow_p (a, fb, bb);	/* { dg-error "argument 2 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_add_overflow_p (a, pb, a);	/* { dg-error "argument 2 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow_p (a, eb, eb);
  x += __builtin_mul_overflow_p (a, bb, bb);
  x += __builtin_add_overflow_p (a, b, fa);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_sub_overflow_p (a, b, ca);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have integral type" } */
  x += __builtin_mul_overflow_p (a, b, c);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have integral type" } */
  return x;
}

int
f4 (float *fp, double *dp, _Complex int *cp, enum E *ep, bool *bp, long long int *llp)
{
  int x = __builtin_add_overflow (1, 2, fp);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_sub_overflow (1, 2, dp);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_mul_overflow (1, 2, cp);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_add_overflow (1, 2, ep);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_sub_overflow (1, 2, bp);	/* { dg-error "argument 3 in call to function\[^\n\r]*does not have pointer to integer type" } */
  x += __builtin_mul_overflow (1, 2, llp);
  return x;
}
