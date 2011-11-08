// { dg-do compile }
// { dg-options "-fgnu-tm -fdump-tree-optimized-asmname" }

struct __attribute__((transaction_safe)) A
{
};

struct B : public A
{
  void f();
};

struct C
{
};

struct D : public C
{
};

struct E : public D, public A
{
  void f();
};

void B::f() { }
void E::f() { }

// { dg-final { scan-tree-dump-times "_ZN1B1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZGTtN1B1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZN1E1fEv" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "_ZGTtN1E1fEv" 1 "optimized" } }
// { dg-final { cleanup-tree-dump "optimized" } }
