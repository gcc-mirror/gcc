static const char s[] = "ab.cd.efghijk";

int
foo (const char *x)
{
  const char *a;
  int b = 0;

  a = __builtin_strchr (s, '.');
  if (a == 0)
    b = 1;
  else if ((a = __builtin_strchr (a + 1, '.')) == 0)
    b = 1;
  else if (__builtin_strncmp (s, x, a - s))
    b = 1;
  else if (__builtin_strncmp (a + 1, x + (a - s + 1), 4) < 0)
    b = 1;

  if (b)
    return 4;
  return 0;
}
