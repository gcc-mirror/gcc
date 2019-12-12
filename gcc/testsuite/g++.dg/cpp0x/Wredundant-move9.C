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

template<typename Tp>
struct T {
  T() { }
  T(const T&) { }
  T(T&&) { }
};

template<typename Tp>
struct U {
  U() { }
  U(const U&) { }
  U(U&&) { }
  U(T<Tp>) { }
};

T<int>
fn1 (T<int> t)
{
  return t;
}

T<int>
fn2 (T<int> t)
{
  // Will use move even without std::move.
  return std::move (t); // { dg-warning "redundant move in return statement" }
}

T<int>
fn3 (const T<int> t)
{
  // t is const: will decay into copy.
  return t;
}

T<int>
fn4 (const T<int> t)
{
  // t is const: will decay into copy despite std::move, so it's redundant.
  // We used to warn about this, but no longer since c++/87378.
  return std::move (t);
}

int
fn5 (int i)
{
  // Not a class type.
  return std::move (i);
}

T<int>
fn6 (T<int> t, bool b)
{
  if (b)
    throw std::move (t);
  return std::move (t); // { dg-warning "redundant move in return statement" }
}

U<int>
fn7 (T<int> t)
{
  // Core 1579 means we'll get a move here.
  return t;
}

U<int>
fn8 (T<int> t)
{
  // Core 1579 means we'll get a move here.  Even without std::move.
  return std::move (t);  // { dg-warning "redundant move in return statement" }
}

T<int>
fn9 (T<int>& t)
{
  // T is a reference and the move isn't redundant.
  return std::move (t);
}

T<int>
fn10 (T<int>&& t)
{
  // T is a reference and the move isn't redundant.
  return std::move (t);
}
