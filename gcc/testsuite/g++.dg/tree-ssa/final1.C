// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-not "vptr" gimple } }

struct A { int i; };
struct B final: public virtual A { int j; };

int f(B* b) { return b->i; }
