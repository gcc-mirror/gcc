/* { dg-do run } */
/* { dg-options "-O3 -fno-partial-inlining -fdump-ipa-cp -fno-devirtualize-speculatively"  } */
/* Main purpose is to verify that we do not produce wrong devirtualization to
   C::m_fn1.  We currently devirtualize to B::m_fn1, so check that. */
#include <stdlib.h>
class A {
public:
  unsigned length;
};
class B {};
class MultiTermDocs : public virtual B {
protected:
  A readerTermDocs;
  A subReaders;
  virtual B *m_fn1(int *) {}
  virtual inline  ~MultiTermDocs();
  inline void wrap(void)
  {
  m_fn1(NULL);
  m_fn1(NULL);
  m_fn1(NULL);
  m_fn1(NULL);
  m_fn1(NULL);
  m_fn1(NULL);
  m_fn1(NULL);
  m_fn1(NULL);
  m_fn1(NULL);
  }
};
class C : MultiTermDocs {
  B *m_fn1(int *);
};
MultiTermDocs::~MultiTermDocs() {
  wrap ();
  if (&readerTermDocs) {
    B *a;
    for (unsigned i = 0; i < subReaders.length; i++)
      (a != 0);
  }
}

B *C::m_fn1(int *) { abort (); }

main()
{
  class C c;
}
/* { dg-final { scan-ipa-dump "Discovered a virtual call to" "cp" { xfail *-*-* } } } */
