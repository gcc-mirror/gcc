/* PR target/120250 */

int a = 5, b = 7, c, *d;
void *l;
void foo (const char *s);
void bar (void);
void qux (const char *, int);

int
baz (int *d, int a, int b)
{
  int c = 0;
  for (int i = 0; i < a; ++i)
    for (int j = 0; j < b; ++j)
      c += d[i * b + j];
  return c;
}

int
main ()
{
  d = __builtin_malloc (a * b * sizeof (int));
  if (!d)
    {
      foo ("foo");
      __builtin_exit (1);
    }
  for (int i = 0; i < a; ++i)
    for (int j = 0; j < b; ++j)
      d[i * b + j] = i * b + j;
  l = (void *) &bar;
  goto *l;
bar:
  c = baz (d, a, b);
  qux ("bar", c);
  __builtin_free (d);
  return 0;
}
