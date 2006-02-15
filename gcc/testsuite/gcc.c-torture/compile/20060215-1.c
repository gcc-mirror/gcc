/* PR middle-end/26300 */

struct S
{
  char c;
  struct S *d;
  struct S *e;
};
extern struct S *u, *v;
extern void fn1 (struct S *) __attribute__ ((noreturn));
void fn2 (struct S *);

static inline struct S *
fn3 (struct S *x)
{
  if (x->c != 6)
    fn1 (0);
  return (struct S *) x;
}

static inline int
fn4 (struct S *x)
{
  if (x != u)
    return 3;
  fn2 (x);
  return 0;
}

int
test (struct S *x)
{
  struct S *r;
  int m = 0;

  for (r = x; r != v; r = (fn3 (r)->d))
    if (r->c != 6)
      fn1 (x);
    else
      m |= 1 << (fn4 (fn3 (r)->e) - 1);
  return m;
}
