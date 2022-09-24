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

struct S1 {
     S1();
     S1(S1 const &) = delete;
     S1(S1 &&);
     S1 operator =(S1 const &) = delete;
     S1 operator =(S1 &&);
};

struct S2 { S2(S1); };
S2 f() {
   S1 s;
   return { std::move(s) }; // { dg-bogus "redundant move" }
}
