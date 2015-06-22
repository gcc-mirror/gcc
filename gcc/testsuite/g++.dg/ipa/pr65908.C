// PR ipa/65908
// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-fPIC" { target fpic } }

class A
{
  A (A &);
};
class B
{
  const A &m_fn1 () const;
};
class C
{
  A m_fn2 () const;
};
A
C::m_fn2 () const
{
  throw 0;
}
const A &
B::m_fn1 () const
{
  throw 0;
}
