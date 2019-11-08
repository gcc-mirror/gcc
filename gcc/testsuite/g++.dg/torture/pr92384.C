// PR c++/92384
// { dg-do run }

struct S {};
struct T : public S { S a, b, c, d, e, f, g, h, i, j, k, l, m; };
struct U { long long a, b, c; };

U
foo (S, S, S, T, T, T, U g)
{
  return g;
}

__attribute__((noipa)) bool
bar (S a, S b, S c, T d, T e, T f, U g, void **h)
{
  h[0] = (void *) &a;
  h[1] = (void *) &b;
  h[2] = (void *) &c;
  h[3] = (void *) &d;
  h[4] = (void *) &e;
  h[5] = (void *) &f;
  h[6] = (void *) &g;
  asm volatile ("" : : "r" (h) : "memory");
  return (h[0] != h[1] && h[1] != h[2] && h[2] != h[3]
	  && h[3] != h[4] && h[4] != h[5] && h[5] != h[6]);
}

int
main ()
{
  S a;
  T b;
  U c = { 1, 2, 3 };
  void *d[7];
  if (!bar (a, a, a, b, b, b, c, d))
    __builtin_abort ();
}
