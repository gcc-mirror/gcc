// PR lto/91574
// { dg-lto-do link }
// { dg-lto-options { { -fPIC -flto -O2 } } }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-extra-ld-options "-shared" }

class A
{
public:
  virtual ~A ();
  A (A &);
  virtual unsigned m_fn1 () const;
};
class B : A
{
  unsigned m_fn1 () const;
};
void
fn1 (B p1)
{
  B a[]{p1, p1};
}
