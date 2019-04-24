/* PR tree-optimization/89998 */

unsigned int sprintf (char *str, const char *fmt, ...);
unsigned int snprintf (char *str, __SIZE_TYPE__ len, const char *fmt, ...);

int
f1 (char *s)
{
  return sprintf (s, "foo");
}

int
f2 (char *s)
{
  return sprintf (s, "%d", 123);
}

int
f3 (int *p, char *s)
{
  const char *t = "bar";
  return sprintf (s, "%s", t);
}

int
f4 (char *s)
{
  return snprintf (s, 8, "foo");
}

int
f5 (char *s)
{
  return snprintf (s, 8, "%d", 123);
}

int
f6 (int *p, char *s)
{
  const char *t = "bar";
  return snprintf (s, 8, "%s", t);
}
