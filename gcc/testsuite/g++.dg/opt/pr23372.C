// PR c++/23372
// { dg-options -fdump-tree-gimple }

// There shouldn't be an assignment to a temporary in the GIMPLE,
// as that represents a redundant copy.
// { dg-final { scan-tree-dump-not "=" gimple } }

struct A {
  int a[1000];
  //A(A const &);
};
void f(A);
void g(A *a) { f(*a); }

