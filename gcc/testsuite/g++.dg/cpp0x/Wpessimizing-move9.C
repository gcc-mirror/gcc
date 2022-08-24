// PR c++/89780
// { dg-do compile { target c++11 } }
// { dg-options "-Wpessimizing-move -Wredundant-move" }

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

struct Dest {
    Dest() = default;
    Dest(Dest&&);
    Dest(const Dest&);
};
struct Source : Dest {};

template <typename T>
Dest withMove() {
  T x;
  return std::move(x);
}

template Dest withMove<Dest>();
template Dest withMove<Source>();

template<typename T>
Dest bar () {
  return std::move(T()); // { dg-warning "moving a temporary object prevents copy elision" }
}

template Dest bar<Dest>();
template Dest bar<Source>();

template<typename T>
Dest baz (T x) {
  return std::move(x);
}

void
call_baz ()
{
  Dest d;
  Source s;
  baz (d);
  baz (s);
}

template<typename>
Dest foo ()
{
  Dest d;
  return std::move(d); // { dg-warning "moving a local object in a return statement prevents copy elision" }
}

template Dest foo<int>();

template<typename>
Dest qux () {
  return std::move(Dest()); // { dg-warning "moving a temporary object prevents copy elision" }
}

template Dest qux<int>();

template<typename>
Dest qui (Dest x) {
  return std::move(x); // { dg-warning "redundant move in return statement" }
}

void
call_qui ()
{
  Dest d;
  qui<int> (d);
}
