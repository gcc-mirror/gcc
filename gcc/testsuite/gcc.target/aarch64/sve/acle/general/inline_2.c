/* { dg-options "-O2" } */

typedef struct s { double d[4]; } TYPE;

static inline void
copy (TYPE *dst, TYPE *src)
{
  __SVFloat64_t tmp = *(__SVFloat64_t *) src;
  *dst = *(TYPE *) &tmp;
}

void
foo (TYPE *a)
{
  copy (a, a + 1);
}
