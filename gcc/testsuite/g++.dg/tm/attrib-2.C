// { dg-do compile }
// { dg-options "-fgnu-tm -fdump-tree-optimized-asmname" }

struct __attribute__((transaction_safe)) Tsafe
{
  void f();
};

void Tsafe::f() { }

struct __attribute__((transaction_callable)) Tcall
{
  void f();
};

void Tcall::f() { }

// { dg-final { scan-tree-dump-times "_ZN5Tsafe1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZN5Tcall1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZGTtN5Tsafe1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZGTtN5Tcall1fEv" 1 "optimized" } }
// { dg-final { cleanup-tree-dump "optimized" } }
