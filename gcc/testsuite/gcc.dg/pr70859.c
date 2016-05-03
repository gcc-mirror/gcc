/* PR c/70859 */
/* { dg-do compile } */

static void *p;
static double *d;
static int r;
__extension__ static _Bool b;

void
fn0 (int n)
{
  p = __builtin_alloca_with_align (n, 6); /* { dg-error "39:must be a constant integer" } */

  r += __builtin_isfinite (0); /* { dg-error "28:non-floating-point argument in call" } */
  r += __builtin_isinf (0); /* { dg-error "25:non-floating-point argument in call" } */
  r += __builtin_isinf_sign (0); /* { dg-error "30:non-floating-point argument in call" } */
  r += __builtin_isnan (0); /* { dg-error "25:non-floating-point argument in call" } */
  r += __builtin_isnormal (0); /* { dg-error "28:non-floating-point argument in call" } */
  r += __builtin_signbit (0); /* { dg-error "27:non-floating-point argument in call" } */

  r += __builtin_isgreater (0, 0); /* { dg-error "8:non-floating-point arguments in call to function" } */
  r += __builtin_isgreaterequal (0, 0); /* { dg-error "8:non-floating-point arguments in call to function" } */
  r += __builtin_isless (0, 0); /* { dg-error "8:non-floating-point arguments in call to function" } */
  r += __builtin_islessequal (0, 0); /* { dg-error "8:non-floating-point arguments in call to function" } */
  r += __builtin_islessgreater (0, 0); /* { dg-error "8:non-floating-point arguments in call to function" } */
  r += __builtin_isunordered (0, 0); /* { dg-error "8:non-floating-point arguments in call to function" } */

  r += __builtin_fpclassify (1, 2, n, 4, 5, n); /* { dg-error "36:non-const integer argument 3 in call" } */
  r += __builtin_fpclassify (1, 2, 3, 4, 5, 6); /* { dg-error "45:non-floating-point argument in call" } */

  d = __builtin_assume_aligned (p, n, p); /* { dg-error "39:non-integer argument 3 in call" } */

  b = __builtin_add_overflow (n, *d, &r); /* { dg-error "34:argument 2 in call to function" } */
  b = __builtin_add_overflow (n, 5, d); /* { dg-error "37:argument 3 in call" } */
  b = __builtin_sub_overflow (n, *d, &r); /* { dg-error "34:argument 2 in call to function" } */
  b = __builtin_sub_overflow (n, 5, d); /* { dg-error "37:argument 3 in call" } */
  b = __builtin_mul_overflow (n, *d, &r); /* { dg-error "34:argument 2 in call to function" } */
  b = __builtin_mul_overflow (n, 5, d); /* { dg-error "37:argument 3 in call" } */
}

int
fn1 (void)
{
  if (__builtin_constant_p ()) /* { dg-error "7:not enough" } */
    return 0;
  if (__builtin_constant_p (1, 2)) /* { dg-error "7:too many" } */
    return 1;
  if (__builtin_isfinite ()) /* { dg-error "7:not enough" } */
    return 3;
  if (__builtin_isfinite (1, 2)) /* { dg-error "7:too many" } */
    return 4;
  if (__builtin_isless (0)) /* { dg-error "7:not enough" } */
    return 5;
  if (__builtin_isless (1, 2, 3)) /* { dg-error "7:too many" } */
    return 6;
  if (__builtin_fpclassify (1, 2, 3, 4, 5)) /* { dg-error "7:not enough" } */
    return 7;
  if (__builtin_fpclassify (1, 2, 3, 4, 5, r, 6)) /* { dg-error "7:too many" } */
    return 8;
  if (__builtin_assume_aligned (p)) /* { dg-error "7:too few" } */
    return 9;
  if (__builtin_assume_aligned (p, r, p, p)) /* { dg-error "7:too many" } */
    return 10;
  if (__builtin_add_overflow ()) /* { dg-error "7:not enough" } */
    return 11;
  if (__builtin_add_overflow (1, 2, 3, &r)) /* { dg-error "7:too many" } */
    return 12;
  return -1;
}
