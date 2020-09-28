/* { dg-additional-options "-O1" } */

struct zx {
  struct zx *b4, *g0;
};

struct oo {
  void *ph;
  struct zx el;
};

inline void
k7 (struct zx *xj)
{
  xj->b4->g0 = 0; /* { dg-warning "dereference of NULL" } */
  xj->b4 = 0;
}

void
n8 (struct oo *yx)
{
  k7 (&yx->el);
  n8 (yx);
}
