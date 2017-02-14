// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

namespace N
{
  class X;
  template <class T>
  class Y
  {
  public:
    inline Y () {}
    inline operator const Y<X> & () const
    {
      return *reinterpret_cast<const Y<X> *>(this);
    }
  };
}
class bar
{
public:
  inline bar () {}
  inline bar (const ::N::Y< ::N::X>& a);
};

class foo
{
  bool b;
public:
  foo();
  void x ()
#if __cplusplus <= 201402L
  throw(bar)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
  ;
};
void foo::x()
#if __cplusplus <= 201402L
throw(bar)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
  if (!b) throw bar (static_cast<::N::X*>(this));	// { dg-error "lambda expressions|expected|invalid" } parse error
}
