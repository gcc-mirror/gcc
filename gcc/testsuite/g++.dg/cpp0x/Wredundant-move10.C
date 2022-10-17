// PR c++/90428
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

struct T { T(); T(const T&); T(T&&) = delete; };
struct S : T { };
struct W { W(const W&); W(W&&) = delete; W(const W&&); };

T f1(T t)
{
  const T& rt = t;
  return std::move(rt); // { dg-warning "redundant move" }
}

T f2(const T& t)
{
  return std::move(t); // { dg-warning "redundant move" }
}

W f3(const W& w)
{
  return std::move(w);
}

T f4(const S& s)
{
  return std::move(s);
}

T f5(const T t)
{
  return std::move(t); // { dg-warning "redundant move" }
}

struct S1 { S1(S1 &&) = delete; S1(const S1&); };
struct S2: S1 {};

S1 f3(const S2 s)
{
  return std::move(s); // { dg-warning "redundant move" }
}
