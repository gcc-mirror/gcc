extern void abort (void);
extern void exit (int);
struct s { unsigned char a[256]; };
union u { struct { struct s b; int c; } d; struct { int c; struct s b; } e; };
static union u v;
static union u v0;
static struct s *p = &v.d.b;
static struct s *q = &v.e.b;

struct outers
{
  struct s inner;
};

static inline struct s rp (void) { return *p; }
static inline struct s rq (void) { return *q; }
static void pq (void)
{
  struct outers o = {rq () };
  *p = o.inner;
}
static void qp (void)
{
  struct outers o = {rp () };
  *q  = o.inner;
}

static void
init (struct s *sp)
{
  int i;
  for (i = 0; i < 256; i++)
    sp->a[i] = i;
}

static void
check (struct s *sp)
{
  int i;
  for (i = 0; i < 256; i++)
    if (sp->a[i] != i)
      abort ();
}

void
main_test (void)
{
  v = v0;
  init (p);
  qp ();
  check (q);
  v = v0;
  init (q);
  pq ();
  check (p);
  exit (0);
}
