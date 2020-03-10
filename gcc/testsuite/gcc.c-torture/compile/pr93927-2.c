/* PR tree-optimization/93927 */

__SIZE_TYPE__ strchr (const char *, int);

char *
foo (char *x)
{
  return !!strchr (x, 0) ? "0" : "1";
}
