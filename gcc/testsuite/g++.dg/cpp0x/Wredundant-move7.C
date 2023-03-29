// PR c++/87378
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

struct S1 { S1(S1 &&); };
struct S2 : S1 {};

S1
f (S2 s)
{
  return std::move(s); // { dg-warning "redundant move in return statement" }
}

struct R1 {
  R1(R1 &&);
  R1(const R1 &&);
};
struct R2 : R1 {};

R1
f2 (const R2 s)
{
  return std::move(s); // { dg-warning "redundant move in return statement" }
}

struct T1 {
  T1(const T1 &);
  T1(T1 &&);
  T1(const T1 &&);
};
struct T2 : T1 {};

T1
f3 (const T2 s)
{
  // Without std::move: const T1 &
  // With std::move: const T1 &&
  return std::move(s); // { dg-warning "redundant move in return statement" }
}
