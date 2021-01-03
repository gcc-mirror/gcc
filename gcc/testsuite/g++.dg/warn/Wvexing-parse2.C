// PR c++/25814
// { dg-do compile { target c++11 } }
// Test -Wvexing-parse.  C++11 features.

struct X { };
struct T {
  T(X);
};

void
fn1 (double (a))
{
  auto l = [](){
    int f(int(a)); // { dg-warning "parentheses were disambiguated as a function declaration" }
  };

  [[noreturn]] int(e)(); // { dg-warning "empty parentheses were disambiguated as a function declaration" }

  T t1{X()};
  T t2(X{});
  T t3{X{}};

  using U = int();
}
