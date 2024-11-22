// PR c++/110137
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized -fno-assume-sane-operators-new-delete" }
// { dg-final { scan-tree-dump "j = 2;" "optimized" } } */
// { dg-final { scan-tree-dump "m = 2;" "optimized" } } */
// { dg-final { scan-tree-dump-times "q = 2;" 2 "optimized" } } */
// { dg-final { scan-tree-dump-times "t = 2;" 2 "optimized" } } */
// { dg-final { scan-tree-dump-not "k = 1;" "optimized" } } */
// { dg-final { scan-tree-dump-not "n = 1;" "optimized" } } */
// { dg-final { scan-tree-dump-not "r = 1;" "optimized" } } */
// { dg-final { scan-tree-dump-not "u = 1;" "optimized" } } */

int i, j, k, l, m, n, o, q, r, s, t, u;

void *
foo ()
{
  i = 1;
  j = 2;
  void *p = ::operator new (32);
  j = 3;
  k = i;
  return p;
}

void
bar (void *p)
{
  l = 1;
  m = 2;
  ::operator delete (p);
  m = 3;
  n = l;
}

int *
baz ()
{
  o = 1;
  q = 2;
  int *p = new int;
  q = 3;
  r = o;
  return p;
}

void
qux (int *p)
{
  s = 1;
  t = 2;
  delete p;
  t = 3;
  u = s;
}

void *
corge ()
{
  o = 1;
  q = 2;
  void *p = __builtin_operator_new (32);
  q = 3;
  r = o;
  return p;
}

void
waldo (void *p)
{
  s = 1;
  t = 2;
  __builtin_operator_delete (p);
  t = 3;
  u = s;
}
