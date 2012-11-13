/* { dg-do run } */

extern void exit (int) __attribute__ ((noreturn));
extern void abort (void) __attribute__ ((noreturn));

struct t
{
  char dummy;
};

struct m
{
  const struct t *t;
  void (*m)(void);
};

struct s
{
  const struct m *m;
  void *o;
};

struct e
{
  const struct t *t;
  void *o;
};

struct ret
{
  struct s s;
  _Bool b;
};

const struct t t1 = { 1 };
const struct t t2 = { 2 };
const struct t t3 = { 3 };
const struct t t4 = { 4 };
const struct t t5 = { 5 };

void
pass (void)
{
  exit (0);
}

void
fail (void)
{
  abort ();
}

const struct m m1 = { &t4, fail };
const struct m m2 = { &t5, pass };

static struct e f2 (struct s s2, void *p);
static struct e f3 (struct s, void *) __attribute__ ((noinline));
static void f4 (struct s, void *) __attribute__ ((noinline));

struct ret c (struct s, const struct t *) __attribute__ ((noinline));

struct ret
c (struct s s1, const struct t *t)
{
  struct ret r;

  if (s1.m->t == t)
    {
      r.s.m = &m2;
      r.s.o = s1.o;
      r.b = 1;
    }
  else
    {
      r.s.m = 0;
      r.s.o = 0;
      r.b = 0;
    }
  return r;
}

void *m (void) __attribute__ ((noinline));

void *
m (void)
{
  return 0;
}

struct e
f1 (struct s s1, void *p)
{
  struct ret r;
  void *a;
  struct s a2;

  r = c (s1, &t5);
  if (r.b)
    return f2 (r.s, p);
  a = m ();
  a2.m = &m1;
  a2.o = a;
  return f2 (a2, p);
}

static struct e
f2 (struct s s2, void *p)
{
  struct e e1;

  e1 = f3 (s2, p);
  if (e1.t == &t2 && e1.o == 0)
    {
      e1.t = 0;
      e1.o = 0;
    }
  return e1;
}

static struct e
f3 (struct s s1, void *p)
{
  struct e r;

  f4 (s1, p);
  r.t = &t3;
  r.o = 0;
  return r;
}

struct s g1;
void *g2;

static void
f4 (struct s s1, void *p)
{
  g1 = s1;
  g2 = p;
  s1.m->m ();
}

int
main ()
{
  struct s s1;

  s1.m = &m2;
  s1.o = 0;
  f1 (s1, 0);
  abort ();
}
