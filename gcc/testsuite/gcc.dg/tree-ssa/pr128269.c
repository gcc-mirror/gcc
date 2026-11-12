/* PR tree-optimization/128269 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target wchar } */

int
foo (__WINT_TYPE__ x, char *b)
{
  if (x > 200)
    return 0;
  int r = __builtin_snprintf (b, 32, "%lc", x);
  return r < 0;
}

int
bar (__WINT_TYPE__ x, char *b)
{
  if (x > 127)
    return 0;
  int r = __builtin_snprintf (b, 32, "%lc", x);
  return r < 0;
}

int
baz (int x, char *b)
{
  if (x < -5 || x > 100)
    return 0;
  int r = __builtin_snprintf (b, 32, "%lc", x);
  return r < 0;
}

int
qux (int x, char *b)
{
  if (x < -5 || x > -1)
    return 0;
  int r = __builtin_snprintf (b, 32, "%lc", x);
  return r < 0;
}

/* The conversion of the ranges [0, 200], [-5, 100] and [-5, -1] can fail,
   so foo, baz and qux keep the test of the result.  The one of [0, 127]
   cannot with an ASCII execution character set, so bar does not use the
   result.  */
/* { dg-final { scan-tree-dump-times "= __builtin_snprintf" 3 "optimized" } } */
