// crash test - XFAIL *-*-*
class X
{
public:
  template <typename A, typename B, typename C>
  X() {}
  
  template <typename A, typename B>
  X::X<A, void, B>() {} // ERROR - non-template type used as a template
};
