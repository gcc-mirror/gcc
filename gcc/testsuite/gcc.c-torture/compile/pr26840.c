extern int f1 (void **);
extern void f2 (void *);

struct s
{
  unsigned char field1;
  int field2;
};

static inline struct s *
get_globals (void)
{
  struct s * r;
  void * rr;

  if (f1 (&rr))
    return 0;
  r = rr;
  if (! r)
    {
      extern struct s t;
      r = &t;
    }
  r->field1 = 1;
  return r;
}

void
atexit_common (const void *dso)
{
  struct s *g = get_globals ();

  if (! g)
    return;
  if (g->field1)
    {
      g->field2 = 0;
      f2 (g);
    }
  else
    f2 (g);
}
