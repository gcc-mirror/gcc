// PR c++/86981
// { dg-do compile { target c++11 } }
// { dg-options "-Wpessimizing-move" }

// Define std::move.
namespace std {
  inline namespace _8 { }
  namespace _8 {
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
}

struct T {
  T() { }
  T(const T&) { }
  T(T&&) { }
};
struct U {
  U() { }
  U(const U&) { }
  U(U&&) { }
  U(T) { }
};

T g;

T
fn1 ()
{
  T t;
  return std::move (t); // { dg-warning "moving a local object in a return statement prevents copy elision" }
}

T
fn2 ()
{
  // Not a local variable.
  return std::move (g);
}

int
fn3 ()
{
  int i = 42;
  // Not a class type.
  return std::move (i);
}

T
fn4 (bool b)
{
  T t;
  if (b)
    throw std::move (t);
  return std::move (t); // { dg-warning "moving a local object in a return statement prevents copy elision" }
}

T
fn5 (T t)
{
  // Function parameter; std::move is redundant but not pessimizing.
  return std::move (t);
}

U
fn6 (T t, U u, bool b)
{
  if (b)
    return std::move (t);
  else
    // Function parameter; std::move is redundant but not pessimizing.
    return std::move (u);
}

U
fn6 (bool b)
{
  T t;
  U u;
  if (b)
    return std::move (t);
  else
    return std::move (u); // { dg-warning "moving a local object in a return statement prevents copy elision" }
}

T
fn7 ()
{
  static T t;
  // Non-local; don't warn.
  return std::move (t);
}

T
fn8 ()
{
  return T();
}

T
fn9 (int i)
{
  T t;

  switch (i)
    {
    case 1:
      return std::move ((t)); // { dg-warning "moving a local object in a return statement prevents copy elision" }
    case 2:
      return (std::move (t)); // { dg-warning "moving a local object in a return statement prevents copy elision" }
    default:
      return (std::move ((t))); // { dg-warning "moving a local object in a return statement prevents copy elision" }
    }
}

int
fn10 ()
{
  return std::move (42);
}
