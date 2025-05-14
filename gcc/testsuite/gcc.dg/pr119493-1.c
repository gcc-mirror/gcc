/* PR tree-optimization/119493 */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-tailr1" } */
/* { dg-final { scan-tree-dump-times " = foo \\\(\[^\n\r;]*\\\);" 4 "tailr1" } } */
/* { dg-final { scan-tree-dump-times " = bar \\\(\[^\n\r;]*\\\);" 4 "tailr1" } } */
/* { dg-final { scan-tree-dump-not " = foo \\\(\[^\n\r;]*\\\); \\\[must tail call\\\]" "tailr1" } } */
/* { dg-final { scan-tree-dump-not " = bar \\\(\[^\n\r;]*\\\); \\\[must tail call\\\]" "tailr1" } } */

struct S { unsigned s; };
struct T { struct S t[2]; };

[[gnu::noinline, gnu::noclone]] struct S
foo (struct S m)
{
  if (m.s == 0 || m.s == 42)
    return m;
  [[gnu::musttail]] return foo ((struct S) { m.s - 1 });
}

[[gnu::noinline, gnu::noclone]] struct S
bar (struct T m, struct S n, int o, int p, int q)
{
  struct T r;
  if (m.t[1].s != o || n.s != o)
    __builtin_abort ();
  if (o == 0 || o == 42)
    return n;
  r = m;
  m.t[1].s -= p;
  r.t[1].s -= q;
  [[gnu::musttail]] return bar (r, m.t[1], o - 1, p, q);
}

int
main ()
{
  if (foo ((struct S) { 0 }).s != 0)
    __builtin_abort ();
  if (foo ((struct S) { 4 }).s != 0)
    __builtin_abort ();
  if (foo ((struct S) { 42 }).s != 42)
    __builtin_abort ();
  if (foo ((struct S) { 51 }).s != 42)
    __builtin_abort ();
  if (bar ((struct T) { { { 0 }, { 0 } } }, (struct S) { 0 }, 0, 1, 1).s != 0)
    __builtin_abort ();
  if (bar ((struct T) { { { 7 }, { 7 } } }, (struct S) { 7 }, 7, 1, 1).s != 0)
    __builtin_abort ();
  if (bar ((struct T) { { { 42 }, { 42 } } },
	   (struct S) { 42 }, 42, 1, 1).s != 42)
    __builtin_abort ();
  if (bar ((struct T) { { { 48 }, { 48 } } },
	   (struct S) { 48 }, 48, 1, 1).s != 42)
    __builtin_abort ();
}
