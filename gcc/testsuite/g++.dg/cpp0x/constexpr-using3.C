// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

namespace ns { typedef int T; }

constexpr int Test1(int x) { using ns::T; typedef T U; return U(x); }
constexpr int Test2(int x) { using namespace ns; typedef T U; return U(x); }
constexpr int Test3(int x) { { using ns::T; typedef T U; return U(x); } }  // { dg-warning "compound-statement" "" { target { c++11_only } } }
constexpr int Test4(int x) { { using namespace ns; typedef T U; return T(x); } }  // { dg-warning "compound-statement" "" { target { c++11_only } } }

struct S1
{
  constexpr S1() { using ns::T; typedef T U; }
};

struct S2
{
  constexpr S2() { using namespace ns; typedef T U; }
};

struct S3
{
  constexpr S3() { { using ns::T; typedef T U; } }  // { dg-warning "compound-statement" "" { target { c++11_only } } }
};

struct S4
{
  constexpr S4() { { using namespace ns; typedef T U; } }  // { dg-warning "compound-statement" "" { target { c++11_only } } }
};
