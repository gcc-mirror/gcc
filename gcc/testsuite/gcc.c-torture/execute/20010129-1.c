long baz1 (void *a)
{
  static long l;
  return l++;
}

int baz2 (const char *a)
{
  return 0;
}

int baz3 (int i)
{
  if (!i)
    abort ();
  return 1;
}

void **bar;

int foo (void *a, long b, int c)
{
  int d = 0, e, f = 0, i;
  char g[256];
  void **h;

  g[0] = '\n';
  g[1] = 0;

  while (baz1 (a) < b) {
    if (g[0] != ' ' && g[0] != '\t') {
      f = 1;
      e = 0;
      if (!d && baz2 (g) == 0) {
	if ((c & 0x10) == 0)
	  continue;
	e = d = 1;
      }
      if (!((c & 0x10) && (c & 0x4000) && e) && (c & 2))
	continue;
      if ((c & 0x2000) && baz2 (g) == 0)
	continue;
      if ((c & 0x1408) && baz2 (g) == 0)
	continue;
      if ((c & 0x200) && baz2 (g) == 0)
	continue;
      if (c & 0x80) {
	for (h = bar, i = 0; h; h = (void **)*h, i++)
	  if (baz3 (i))
	    break;
      }
      f = 0;
    }
  }
  return 0;
}

int main ()
{
  void *n = 0;
  bar = &n;
  foo (&n, 1, 0xc811);
  exit (0);
}
