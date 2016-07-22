template <typename T> struct A
{
  static const int t[1][1]={{0}}; // { dg-error "20:'constexpr' needed" "" { target c++11 } }
  // { dg-error "20:invalid in-class" "" { target c++98_only } 3 }
  // { dg-error "28:a brace-enclosed" "" { target c++98_only } 3 }
};
