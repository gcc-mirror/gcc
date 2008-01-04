static const char s[] = "ab.cd.efghijk";
static const char t[] = "abcde";

long
foo (const char *x)
{
  const char *a;
  long b = 0;

  a = __builtin_strchr (s, '.');
  return ((long) a) + (1 - (long) t);
}
