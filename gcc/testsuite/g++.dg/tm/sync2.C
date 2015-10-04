// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm -fdump-tree-optimized-asmname" }

struct Tsafe
{
  void f() transaction_safe;
};

void Tsafe::f() { }

struct Tcall
{
  [[optimize_for_synchronized]] void f();
};

void Tcall::f() { }

// { dg-final { scan-tree-dump-times "_ZN5Tsafe1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZN5Tcall1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZGTtN5Tsafe1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZGTtN5Tcall1fEv" 1 "optimized" } }
