// P0847R7
// { dg-do compile { target c++23 } }

// diagnosis of ill-formed calls to by-value xobj member functions
// due to an absence of valid conversion functions

struct NotFromS {};

struct S {
  void f(this int) {}
  void g(this NotFromS) {}
};

void test()
{
  S s{};
  s.f(); // { dg-error {cannot convert 'S' to 'int'} }
  s.g(); // { dg-error {cannot convert 'S' to 'NotFromS'} }
}

