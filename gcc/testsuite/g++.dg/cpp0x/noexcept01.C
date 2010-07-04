// Test for noexcept-expression
// { dg-options "-std=c++0x -O2" }

#include <typeinfo>

#define SA(X) static_assert(X, #X)

void f();
void g() throw();
SA(noexcept(g()));
SA(!noexcept(f()));
SA(!noexcept(throw 1));
SA(noexcept(42));

struct A
{
  virtual ~A();
};

struct B: public A
{
  virtual ~B();
};

A* ap;

struct C { };
C* cp;

SA (noexcept (dynamic_cast<B*>(ap)));
SA (!noexcept (dynamic_cast<B&>(*ap)));
SA (!noexcept (typeid (*ap)));
SA (noexcept (typeid (*cp)));

SA (!noexcept (true ? 1 : throw 1));
SA (!noexcept (true || true ? 1 : throw 1));

SA (noexcept (C()));

struct D
{
  D() throw();
};

SA (noexcept (D()));

struct E
{
  E() throw();
  ~E();
};

SA (!noexcept (E()));

struct F
{
  virtual void f();
};

SA (noexcept (F()));

struct G
{
  G() = default;
  ~G() = default;
};

SA (noexcept (G()));

template <class T, bool b>
void tf()
{
  SA (noexcept (T()) == b);
}

template void tf<int,true>();
template void tf<E, false>();

// Make sure that noexcept uses the declared exception-specification, not
// any knowledge we might have about whether or not the function really
// throws.
void h() { }
SA(!noexcept(h()));
