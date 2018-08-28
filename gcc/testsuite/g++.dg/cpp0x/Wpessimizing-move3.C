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
struct U { U(T); };

template<typename Tp>
T
fn1 ()
{
  T t;
  // Non-dependent type.
  return std::move (t); // { dg-warning "moving a local object in a return statement prevents copy elision" }
}

template<typename Tp1, typename Tp2>
Tp1
fn2 ()
{
  Tp2 t;
  return std::move (t); // { dg-warning "moving a local object in a return statement prevents copy elision" }
}

template<typename Tp1, typename Tp2>
Tp1
fn3 ()
{
  Tp2 t;
  return std::move (t);
}

int
main ()
{
  fn1<T>();
  fn2<T, T>();
  fn3<U, T>();
}
