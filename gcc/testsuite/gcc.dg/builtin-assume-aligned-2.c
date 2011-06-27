/* { dg-do compile } */

double *bar (void);

void
foo (double *ptr, int i)
{
  double *a = __builtin_assume_aligned (ptr, 16, 8, 7);	/* { dg-error "too many arguments to function" } */
  double *b = __builtin_assume_aligned (bar (), 16);
  double *c = __builtin_assume_aligned (bar (), 16, 8);
  double *d = __builtin_assume_aligned (ptr, i, ptr);	/* { dg-error "non-integer argument 3 in call to function" } */
  double *e = __builtin_assume_aligned (ptr, i, *ptr);	/* { dg-error "non-integer argument 3 in call to function" } */
  *a = 0.0;
  *b = 0.0;
  *c = 0.0;
  *d = 0.0;
  *e = 0.0;
}
