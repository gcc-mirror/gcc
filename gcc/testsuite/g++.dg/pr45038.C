// PR preprocessor/45038
// { dg-do compile }
// { dg-options "-Werror -Wold-style-cast" }

double f(void)
{
  // We used to produce old-style casts for this.
  return __DBL_MIN__;
}
