// PR c++/61982
// { dg-additional-options "-O2 -fdump-tree-optimized -fdelete-null-pointer-checks" }
// { dg-final { scan-tree-dump-not "= 0" "optimized" } }

struct X { 
  int i; 
  void clear() { i = 0; }
}; 

void f(X* x) { 
  x->clear(); 
  x->~X(); 
} 

void g(X* x) {
  x->clear();
  delete x;
}
