// Build don't link:
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
  void x () throw(bar);
};
void foo::x() throw(bar)
{
  if (!b) throw bar (static_cast<::N::X*>(this));	// ERROR - parse error
}
