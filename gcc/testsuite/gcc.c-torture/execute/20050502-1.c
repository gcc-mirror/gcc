/* PR rtl-optimization/21330 */

extern void abort (void);
extern int strcmp (const char *, const char *);

int
__attribute__((noinline))
bar (const char **x)
{
  return *(*x)++;
}

int
__attribute__((noinline))
baz (int c)
{
  return c != '@';
}

void
__attribute__((noinline))
foo (const char **w, char *x, _Bool y, _Bool z)
{
  char c = bar (w);
  int i = 0;

  while (1)
    {
      x[i++] = c;
      c = bar (w);
      if (y && c == '\'')
        break;
      if (z && c == '\"')
        break;
      if (!y && !z && !baz (c))
        break;
    }
   x[i] = 0;
}

int
main (void)
{
  char buf[64];
  const char *p;
  p = "abcde'fgh";
  foo (&p, buf, 1, 0);
  if (strcmp (p, "fgh") != 0 || strcmp (buf, "abcde") != 0)
    abort ();
  p = "ABCDEFG\"HI";
  foo (&p, buf, 0, 1);
  if (strcmp (p, "HI") != 0 || strcmp (buf, "ABCDEFG") != 0)
    abort ();
  p = "abcd\"e'fgh";
  foo (&p, buf, 1, 1);
  if (strcmp (p, "e'fgh") != 0 || strcmp (buf, "abcd") != 0)
    abort ();
  p = "ABCDEF'G\"HI";
  foo (&p, buf, 1, 1);
  if (strcmp (p, "G\"HI") != 0 || strcmp (buf, "ABCDEF") != 0)
    abort ();
  p = "abcdef@gh";
  foo (&p, buf, 0, 0);
  if (strcmp (p, "gh") != 0 || strcmp (buf, "abcdef") != 0)
    abort ();
  return 0;
}
