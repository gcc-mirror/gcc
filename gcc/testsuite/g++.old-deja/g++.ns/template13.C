// Build don't link:
// Templates defined outside must be declared inside
namespace bar
{
  template<class T>
  void foo(); // trick it to provide some prior declaration
  template<class T>class X;
}

template <typename T>
T const
bar::foo(T const &a)    
{                        // ERROR - not declared in bar - XFAIL *-*-*
  return a;
}

template<> void bar::foo<int>()
{                        // ERROR - not declared in bar - XFAIL *-*-*
}

template<class T,class U>
class bar::X{};         // ERROR - does not match declaration - XFAIL *-*-*
