// PR c++/25814
// { dg-do compile }
// Test -Wvexing-parse in a template.

struct X { };

template<typename T>
void fn ()
{
  T t(); // { dg-warning "empty parentheses were disambiguated as a function declaration" }
  T a(X()); // { dg-warning "parentheses were disambiguated as a function declaration" }
  X x(T()); // { dg-warning "parentheses were disambiguated as a function declaration" }
  int i(T()); // { dg-warning "parentheses were disambiguated as a function declaration" }
}
