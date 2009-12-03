/* PR middle-end/42049 */

extern char *strcpy (char *s1, const char *s2);
struct S { char s[4]; };

int
foo (int x, char **y)
{
  char const *a;
  char const *b;
  struct S s[9];
  long i;
  if (x > 1)
    a = y[1];
  else
    a = "abc";
  if (x > 2)
    b = y[2];
  else
    b = "def";
  strcpy (s[0].s, a);
  strcpy (s[1].s, b);
  for (i = 2; i < x - 2 && i < 8; i++)
    strcpy (s[i].s, y[i + 1]);
  s[i].s[0] = '\0';
  return 0;
}
