/* { dg-do link } */
/* { dg-options "-ffinite-math-only" } */

float f;

int main()
{
  if (__builtin_isunordered (f, f) != 0)
    link_error ();
  if (__builtin_isnan (f) != 0)
    link_error ();
  if (__builtin_finite (f) != 1)
    link_error ();
  if (f != f)
    link_error ();
  return 0;
}
