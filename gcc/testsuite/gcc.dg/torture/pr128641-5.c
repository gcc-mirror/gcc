/* The accesses to struct A and struct B deliberately alias.  Check that
   predictive commoning does not use an initializer with a different alias
   set from the chain root.  */
/* { dg-do run } */

extern void *calloc (__SIZE_TYPE__, __SIZE_TYPE__);

struct A { long x; };
struct B { long x; };

int n, sink;

__attribute__((noinline)) void
g (struct A *p)
{
  p->x = 3;
}

__attribute__((noinline)) void
f (struct B *p)
{
  p->x = 7;
}

int
main (void)
{
  struct B *b = calloc (10, sizeof (struct B));
  g ((struct A *) b);
  long prev = ((struct A *) b)->x;
  f (b);
  long s = 0, t = 0;
  for (int k = 0; k < (n & 7) + 1; k++)
    {
      long cur = b[k + 1].x;
      s = s * 31 + b[k].x;
      t += prev;
      prev = cur;
      sink = k;
    }
  if (s != 7 || t != 3)
    __builtin_abort ();
  return 0;
}
