// PR c++/48594
// Test for uses of (X->*Y)() that don't actually involve a
// pointer to member function.

struct A { } a;
struct B { } b;
struct C * cp;

struct Func { void operator()(); };
Func operator->* (A, int);

typedef void (*pfn)();
pfn operator->* (B, int);

pfn C::*cpfn;
Func C::*cfunc;

template <class T>
void f()
{
  (a->*1)();
  (b->*1)();
  (cp->*cpfn)();
  (cp->*cfunc)();
}
