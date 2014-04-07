// { dg-do compile }
// { dg-options "-O3" }

class ASN1Object
{
public:
  virtual ~ASN1Object ();
};
class A
{
  virtual unsigned m_fn1 () const;
};
class B
{
public:
  ASN1Object Element;
  virtual unsigned m_fn1 (bool) const;
};
template <class BASE> class C : public BASE
{
};

class D : ASN1Object, public B
{
};
class G : public D
{
  unsigned m_fn1 (bool) const {}
};
class F : A
{
public:
  F (A);
  unsigned m_fn1 () const
  {
    int a;
    a = m_fn2 ().m_fn1 (0);
    return a;
  }
  const B &m_fn2 () const { return m_groupParameters; }
  C<G> m_groupParameters;
};
template <class D> void BenchMarkKeyAgreement (int *, int *, int)
{
  A f;
  D d (f);
}

void BenchmarkAll2 () { BenchMarkKeyAgreement<F>(0, 0, 0); }

