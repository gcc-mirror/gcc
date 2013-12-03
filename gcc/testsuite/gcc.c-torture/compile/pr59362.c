/* PR tree-optimization/59362 */

char *
foo (char *r, int s)
{
  r = __builtin___stpcpy_chk (r, "abc", __builtin_object_size (r, 1));
  if (s)
    r = __builtin___stpcpy_chk (r, "d", __builtin_object_size (r, 1));
  return r;
}

char *a;
long int b;

void
bar (void)
{
  b = __builtin_object_size (0, 0);
  a = __builtin___stpcpy_chk (0, "", b);
  b = __builtin_object_size (a, 0);
}
