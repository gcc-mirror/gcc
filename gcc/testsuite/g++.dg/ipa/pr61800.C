/* PR ipa/61800 */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-options "-O2 -Wno-return-type" } */

#pragma GCC visibility push(hidden)
class A
{
public:
  unsigned long m_fn1 () const;
};
class B;
class C
{
public:
  ;
  virtual bool m_fn2 (void) = 0;
};
class D
{
  virtual bool m_fn3 (const int &p1, B *p2) = 0;
};
class F : public D
{
  bool m_fn3 (const int &p1, B *p2);
  A mPredicates;
};
class B
{
};
class G : public B
{
  virtual unsigned int m_fn4 () = 0;
};
class H : public G
{
public:
  int txNodeSetContext_aContextNodeSet;
  H (B *p1) {}
  int
  m_fn5 ()
  {
    return mPosition < m_fn4 ();
  }
  unsigned int m_fn4 ();
  unsigned int mPosition;
};

unsigned int a;
C *b;
bool
F::m_fn3 (const int &p1, B *p2)
{
  if (!b->m_fn2 ())
    return false;
  unsigned int c = mPredicates.m_fn1 ();
  for (1; 1 < c; ++a)
    {
      H d (p2);
      while (d.m_fn5 ())
        {
          do
            {
            }
          while (0);
        }
    }
}
