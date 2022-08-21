// PR c++/106276
// { dg-do compile { target c++11 } }
// { dg-options "-Wpessimizing-move" }

// Define std::move.
namespace std {
  template<typename _Tp>
    struct remove_reference
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp   type; };

  template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
}

struct A { A(); A(const A&) = delete; A(A&&); };
struct B { B(A); };
struct X { };

void foo (A);
void bar (X);

void
fn1 ()
{
  A a1 = std::move (A()); // { dg-warning "moving a temporary object prevents copy elision" }
  A a2 = std::move (A{}); // { dg-warning "moving a temporary object prevents copy elision" }
  A a3(std::move (A())); // { dg-warning "moving a temporary object prevents copy elision" }
  A a4(std::move (A{})); // { dg-warning "moving a temporary object prevents copy elision" }
  A a5{std::move (A())}; // { dg-warning "moving a temporary object prevents copy elision" }
  A a6{std::move (A{})}; // { dg-warning "moving a temporary object prevents copy elision" }
  A a7 = {std::move (A())}; // { dg-warning "moving a temporary object prevents copy elision" }
  A a8 = {std::move (A{})}; // { dg-warning "moving a temporary object prevents copy elision" }

  B b1 = std::move (A()); // { dg-warning "moving a temporary object prevents copy elision" }
  B b2(std::move (A())); // { dg-warning "moving a temporary object prevents copy elision" }
  B b3{std::move (A())}; // { dg-warning "moving a temporary object prevents copy elision" }
  B b4 = {std::move (A())}; // { dg-warning "moving a temporary object prevents copy elision" }

  X x1 = std::move (X()); // { dg-warning "moving a temporary object prevents copy elision" }
  X x2 = std::move (X{}); // { dg-warning "moving a temporary object prevents copy elision" }
  X x3(std::move (X())); // { dg-warning "moving a temporary object prevents copy elision" }
  X x4(std::move (X{})); // { dg-warning "moving a temporary object prevents copy elision" }
  X x5{std::move (X())}; // { dg-warning "moving a temporary object prevents copy elision" }
  X x6{std::move (X{})}; // { dg-warning "moving a temporary object prevents copy elision" }
  X x7 = {std::move (X())}; // { dg-warning "moving a temporary object prevents copy elision" }
  X x8 = {std::move (X{})}; // { dg-warning "moving a temporary object prevents copy elision" }

  foo (std::move (A())); // { dg-warning "moving a temporary object prevents copy elision" }
  foo (std::move (A{})); // { dg-warning "moving a temporary object prevents copy elision" }
  bar (std::move (X())); // { dg-warning "moving a temporary object prevents copy elision" }
  bar (std::move (X{})); // { dg-warning "moving a temporary object prevents copy elision" }

  foo (std::move (a1));
  bar (std::move (x1));
}
