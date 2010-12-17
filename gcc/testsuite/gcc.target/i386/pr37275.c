/* PR middle-end/37275 */
/* { dg-do compile { target ilp32 } } */
/* { dg-options "-g -dA -O2 -march=i686 -fstack-protector" } */
/* { dg-require-visibility "" } */

typedef __SIZE_TYPE__ size_t;
extern void *memcpy (void *, const void *, size_t);
extern void *malloc (size_t);

typedef int A;

struct B
{
  int x;
};

struct C
{
  struct F *c1;
  void *c2;
};

enum D
{
  D0,
  D1
};

struct E
{
  struct E *e1;
  struct E *e2;
  struct B e3;
  void (*fn) (void *);
  void *fn_data;
  enum D e4;
  _Bool e5;
  _Bool e6;
};

struct F
{
  unsigned f1;
  A f2;
  int f3;
};

struct G
{
  void (*fn) (void *data);
  void *data;
  struct C g1;
  struct E *t;
};

extern void fn1 (A * m);
static inline void
fn2 (A *x)
{
  if (!__sync_bool_compare_and_swap (x, 0, 1))
    fn1 (x);
}

extern __thread struct G thr __attribute__ ((visibility ("hidden")));
static inline struct G *
fn3 (void)
{
  return &thr;
}

extern struct B *fn4 (void);
extern struct B a;

static inline struct B *
fn5 (_Bool x)
{
  struct E *t = fn3 ()->t;
  if (t)
    return &t->e3;
  else if (x)
    return fn4 ();
  else
    return &a;
}

void
fn6 (struct E *t, struct E *e1_t,
		struct B *prev_e3)
{
  t->e1 = e1_t;
  t->e3 = *prev_e3;
  t->e4 = D0;
  t->e5 = 0;
  t->e6 = 0;
  t->e2 = ((void *) 0);
}

void
test (void (*fn) (void *), void *data, void (*cpyfn) (void *, void *), long x, long y, _Bool z)
{
  struct G *thr = fn3 ();
  struct F *c1 = thr->g1.c1;
  if (!z || c1 == 0 || (unsigned) c1->f3 > 64 * c1->f1)
    {
      struct E t;

      fn6 (&t, thr->t, fn5 (0));
      if (thr->t)
	t.e6 = thr->t->e6;
      thr->t = &t;
      if (__builtin_expect (cpyfn != ((void *) 0), 0))
	{
	  char buf[x + y - 1];
	  char *arg = (char *) (((unsigned long) buf + y - 1)
				& ~(unsigned long) (y - 1));
	  cpyfn (arg, data);
	  fn (arg);
	}
    }
  else
    {
      struct E *t;
      struct E *e1 = thr->t;
      char *arg;

      t = malloc (sizeof (*t) + x + y - 1);
      arg = (char *) (((unsigned long) (t + 1) + y - 1)
		      & ~(unsigned long) (y - 1));
      fn6 (t, e1, fn5 (0));
      thr->t = t;
      if (cpyfn)
	cpyfn (arg, data);
      else
	memcpy (arg, data, x);
      thr->t = e1;
      fn2 (&c1->f2);
    }
}
