// { dg-do compile }
// Templates defined outside must be declared inside
namespace bar
{
  // trick it to provide some prior declaration
  template<class T>
  void foo(); // { dg-error "definition" }
  template<class T>class X; // { dg-message "note: previous declaration" }
}

template <typename T>
T const
bar::foo(T const &a)     // { dg-error "" "" { xfail *-*-* } } not declared in bar - 
{
  return a;
}

template<> void bar::foo<int>()     // { dg-error "different namespace" }
{
}

template<class T,class U>
class bar::X{};         // { dg-error "redeclared with 2 template parameter" }
