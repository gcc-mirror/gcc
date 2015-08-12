/* { dg-do compile } */
/* { dg-options "-Os -fcheck-pointer-bounds -mmpx" } */

typedef void (func) (int *);

static inline void
bar (func f)
{
  int i;
  f (&i);
}

void
foo ()
{
  bar (0);
}
