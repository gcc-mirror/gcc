// Build don't link:
// Templates defined outside must be declared inside
namespace bar
{
  template<class T>class X;
}

template <typename T>
T const
bar::foo(T const &a)    
{                        // ERROR - not declared in bar - XFAIL *-*-*
  return a;
}

template<> const int bar::foo<int>(int const &)
{                        // ERROR - not declared in bar - XFAIL *-*-*
  return 0;
}

template<class T,class U>
class bar::X{};         // ERROR - does not match declaration - XFAIL *-*-*
