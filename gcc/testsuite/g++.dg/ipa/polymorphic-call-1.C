// { dg-do compile }
// { dg-options "-O2" }
class A;
class B
{
  A *mRawPtr;

public:
  void *StartAssignment___trans_tmp_2;
  A **
  m_fn1 ()
  {
    StartAssignment___trans_tmp_2 = &mRawPtr;
    return reinterpret_cast<A **> (StartAssignment___trans_tmp_2);
  }
};
class C
{
public:
  C (B &p1) : mTargetSmartPtr (p1) {}
  operator A **() { return mTargetSmartPtr.m_fn1 (); }
  B &mTargetSmartPtr;
};
class A
{
public:
  A ();
};
class D
{
  D (bool);
  B mNewEntry;
  virtual int m_fn2 ();
};
C
fn1 (B &p1)
{
  return p1;
}
void
fn2 (bool, A **)
{
  new A;
}
D::D (bool p1)
{
  A **a = fn1 (mNewEntry);
  fn2 (p1, a);
}
