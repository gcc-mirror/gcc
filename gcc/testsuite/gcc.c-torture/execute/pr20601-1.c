/* PR tree-optimization/20601 */
extern void abort (void);
extern void exit (int);

struct T
{
  char *t1;
  char t2[4096];
  char **t3;
};

int a[5];
int b;
char **c;
int d;
char **e;
struct T t;
char *f[16];
char *g[] = { "a", "-u", "b", "c" };

__attribute__ ((__noreturn__)) void
foo (void)
{
  while (1);
}

__attribute__ ((noinline)) char *
bar (char *x, unsigned int y)
{
  return 0;
}

static inline char *
baz (char *x, unsigned int y)
{
  if (sizeof (t.t2) != (unsigned int) -1 && y > sizeof (t.t2))
    foo ();
  return bar (x, y);
}

static inline int
setup1 (int x)
{
  char *p;
  int rval;

  if (!baz (t.t2, sizeof (t.t2)))
    baz (t.t2, sizeof (t.t2));

  if (x & 0x200)
    {
      char **h, **i = e;

      ++d;
      e = f;
      if (t.t1 && *t.t1)
        e[0] = t.t1;
      else
        abort ();

      for (h = e + 1; (*h = *i); ++i, ++h)
        ;
    }
  return 1;
}

static inline int
setup2 (void)
{
  int j = 1;

  e = c + 1;
  d = b - 1;
  while (d > 0 && e[0][0] == '-')
    {
      if (e[0][1] != '\0' && e[0][2] != '\0')
        abort ();

      switch (e[0][1])
        {
        case 'u':
          if (!e[1])
            abort ();

          t.t3 = &e[1];
          d--;
          e++;
          break;
        case 'P':
          j |= 0x1000;
          break;
        case '-':
          d--;
          e++;
          if (j == 1)
            j |= 0x600;
          return j;
        }
      d--;
      e++;
    }

  if (d > 0 && !(j & 1))
    abort ();

  return j;
}

int
main (void)
{
  int x;
  c = g;
  b = 4;
  x = setup2 ();
  t.t1 = "/bin/sh";
  setup1 (x);
  /* PRE shouldn't transform x into the constant 0x601 here, it's not legal.  */
  if ((x & 0x400) && !a[4])
    abort ();
  exit (0);
}
