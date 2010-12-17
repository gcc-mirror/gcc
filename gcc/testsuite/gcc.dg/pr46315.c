/* PR rtl-optimization/46315 */
/* Reported by Magnus Granberg <zorry@gentoo.org> */

/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-overflow" } */

extern void abort (void);

static char const *
parse_ranged (char const *s, int digits)
{
  int n = 0;
  char const *lim = s + digits;
  while (s < lim)
    {
      unsigned d = *s++ - '0';
      if (9 < d)
        return 0;
      n = 10 * n + d;
    }
  return s && 0 <= n && n <= 59 ? s : 0;
}

int main(void)
{
  const char *s = "10092240";

  s = parse_ranged (s, 2);
  s = parse_ranged (s, 2);
  s = parse_ranged (s, 2);
  s = parse_ranged (s, 2);

  if (!s || *s != '\0')
    abort();

  return 0;
}
