/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/121355 */

#define array_size 2
struct s1
{
  int t[array_size];
};

struct s2
{
  int p;
  struct s1 t;
};
static inline int *b(struct s1 *t)
{
  return t->t;
}
static inline int *e(struct s1 *t)
{
  return b(t) + array_size;
}
void g(struct s2 *t)
{
  struct s1 *t2 = &t->t;
  int *te = e(t2);
  int *ts = b(t2);
  int tt = te - ts;
/*
  _6 = t_3(D) + 12;
  _8 = &MEM[(struct s1 *)t_3(D) + 4B].t;
  _1 = _6 - _8;

  _1 should be optimized to 2*sizeof(int) == 8.
 */

  if (tt != array_size)
    __builtin_abort();
}

/* the call to abort should be removed. */

/* { dg-final { scan-tree-dump-not "abort " "optimized" } } */
