/* { dg-do link } */
/* { dg-options "-ffinite-math-only" } */

extern void link_error(void);

float f;
double d;
long double ld;

int main()
{
  if (__builtin_isunordered (f, f) != 0)
    link_error ();
  if (__builtin_isunordered (d, d) != 0)
    link_error ();
  if (__builtin_isunordered (ld, ld) != 0)
    link_error ();

  if (__builtin_isnan (f) != 0)
    link_error ();
  if (__builtin_isnan (d) != 0)
    link_error ();
  if (__builtin_isnan (ld) != 0)
    link_error ();
  if (__builtin_isnanf (f) != 0)
    link_error ();
  if (__builtin_isnanl (ld) != 0)
    link_error ();

  if (__builtin_finite (f) != 1)
    link_error ();
  if (__builtin_finite (d) != 1)
    link_error ();
  if (__builtin_finite (ld) != 1)
    link_error ();
  if (__builtin_finitef (f) != 1)
    link_error ();
  if (__builtin_finitel (ld) != 1)
    link_error ();

  if (__builtin_isinf (f) != 0)
    link_error ();
  if (__builtin_isinf (d) != 0)
    link_error ();
  if (__builtin_isinf (ld) != 0)
    link_error ();

  if (__builtin_isfinite (f) != 1)
    link_error ();
  if (__builtin_isfinite (d) != 1)
    link_error ();
  if (__builtin_isfinite (ld) != 1)
    link_error ();

  if (f != f)
    link_error ();
  if (d != d)
    link_error ();
  if (ld != ld)
    link_error ();
  return 0;
}
