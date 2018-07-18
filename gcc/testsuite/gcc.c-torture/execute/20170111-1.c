/* PR rtl-optimization/79032 */
/* Reported by Daniel Cederman <cederman@gaisler.com> */

extern void abort (void);

struct S {
  short a;
  long long b;
  short c;
  char d;
  unsigned short e;
  long *f;
};

static long foo (struct S *s) __attribute__((noclone, noinline));

static long foo (struct S *s)
{
  long a = 1;
  a /= s->e;
  s->f[a]--;
  return a;
}

int main (void)
{
  long val = 1;
  struct S s = { 0, 0, 0, 0, 2, &val };
  val = foo (&s);
  if (val != 0)
    abort ();
  return 0;
}
