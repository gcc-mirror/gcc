// PR c++/86981
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

struct T { };

T
fn1 ()
{
  T t;
  return (1, std::move (t));
}

T
fn2 ()
{
  T t;
  return [&](){ return std::move (t); }();
}

T
fn3 ()
{
  T t;
  return [=](){ return std::move (t); }();
}
