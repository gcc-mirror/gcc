/* { dg-additional-options "-O1" } */

struct cg {
  int hk;
  int *bg;
};

union vb {
  struct cg gk;
};

void
l3 (union vb *);

void
pl (void)
{
  union vb th = { 0, };
  int sc;

  for (sc = 0; sc < 1; ++sc)
    {
      th.gk.hk = 0;
      th.gk.bg[sc] = 0; /* { dg-warning "uninitialized" } */
      l3 (&th);
    }
}
