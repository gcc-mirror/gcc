// PR c++/94568
// { dg-do compile { target c++20 } }

struct A;
typedef int A::*MemPtr;

struct B { MemPtr p; };

static constexpr MemPtr mp { };

template <B> struct X { };

typedef X<B{{MemPtr{ }}}> XB;
typedef X<B{{mp}}>        XB;

struct C { int a[2]; };
template <C> struct D { };

constexpr const int i0 = 0;
constexpr const int i_{ };

static_assert (i0 == i_);

// typedef D<C{ { 0, 1 } }>   DC01;
// typedef D<C{ { i0, 1 } }>  DC01;
typedef D<C{ { i_, 1 } }>  DC01;

// { dg-final { scan-assembler "_Z1f1DIXtl1CtlA2_iLi0ELi1EEEEE" } }
void f(DC01) {}
