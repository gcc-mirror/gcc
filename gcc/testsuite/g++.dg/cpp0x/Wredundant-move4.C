// PR c++/87029
// { dg-do compile { target c++11 } }
// { dg-options "-Wredundant-move" }

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

U
fn1 (T t, bool b)
{
  if (b)
    return t;
  else
    return std::move (t); // { dg-warning "redundant move in return statement" }
}

U
fn2 (bool b)
{
  T t;
  if (b)
    return t;
  else
    return std::move (t); // { dg-warning "redundant move in return statement" }
}

U
fn3 (bool b)
{
  static T t;
  if (b)
    return t;
  else
    return std::move (t);
}

T g;

U
fn4 (bool b)
{
  if (b)
    return g;
  else
    return std::move (g);
}

long int
fn5 (bool b)
{
  int i = 42;
  if (b)
    return i;
  else
    return std::move (i);
}
