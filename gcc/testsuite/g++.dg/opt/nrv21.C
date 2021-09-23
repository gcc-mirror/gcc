// PR c++/67302
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-not "<retval> = a" "gimple" } }

struct A
{
  int ar[42];
  A();
};

A f() {
  A a;
  return (a); // The parens should not inhibit NRVO.
}
