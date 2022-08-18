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

struct T { };
struct U { U(T); };

template<typename Tp>
T
fn1 (T t)
{
  // Non-dependent type.
  return std::move (t); // { dg-warning "redundant move in return statement" }
}

template<typename Tp1, typename Tp2>
Tp1
fn2 (Tp2 t)
{
  return std::move (t);
}

template<typename Tp1, typename Tp2>
Tp1
fn3 (Tp2 t)
{
  return std::move (t);
}

int
main ()
{
  T t;
  fn1<T>(t);
  fn2<T, T>(t);
  fn3<U, T>(t);
}
