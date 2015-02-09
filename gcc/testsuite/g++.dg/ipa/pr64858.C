// { dg-do compile }
// { dg-options "-O2 -std=gnu++11" }

template <class reference_type> class A
{
  reference_type *m_pBody;
public:
  A (const A &) { m_pBody->acquire (); }
};
class B;
class C
{
protected:
  B *_pInterface;
};
template <class interface_type> class I : C
{
public:
  I (interface_type *);
};
class B
{
public:
  virtual void acquire ();
};
class D
{
protected:
  void acquire ();
};
template <class Ifc1> class J : D, public Ifc1
{
  void
  acquire ()
  {
    D::acquire ();
  }
};
class K : B
{
};
class L;
class F
{
  A<L> m_pDocument;
  F (A<L> const &, int &&);
};
class XUnoTunnel;
class XEventTarget;
template <class, class> class WeakImplHelper3 : D, B
{
  void
  acquire ()
  {
    D::acquire ();
  }
};
template <class> class G
{
public:
  void
  acquire ()
  {
    WeakImplHelper3<XUnoTunnel, XEventTarget> ();
  }
};
struct H
{
  H ()
      : mxAttribList (new J<B>), mxCurrentHandler (0), mxDocHandler (0),
        mxTokenHandler (0)
  {
  }
  I<J<B> > mxAttribList;
  I<int> mxCurrentHandler;
  I<int> mxDocHandler;
  I<int> mxTokenHandler;
};
class L : public G<int>
{
};
class M : public J<K>
{
public:
  M ();
};
template <class interface_type> I<interface_type>::I (interface_type *p1)
{
  B *a = static_cast<B *> (static_cast<void *> (p1));
  _pInterface = a;
  _pInterface->acquire ();
}
F::F (A<L> const &p1, int &&) : m_pDocument (p1) { I<K> (new M); }
