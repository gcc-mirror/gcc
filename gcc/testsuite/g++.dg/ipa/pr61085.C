/* { dg-do run } */
/* { dg-options "-O2 -fno-early-inlining" } */

struct A {};
struct B : virtual A {
  unsigned m_i;
  B() : m_i () {}
  virtual A *m_virt ()
  {
    return 0;
  }
  ~B ()
  {
    m_foo ();
    while (m_i)
      ;
  }
  void m_foo ()
  {
    m_virt ();
  }
};

class C : B {
  A *m_virt () {
    __builtin_abort ();
  }
};

int main ()
{
  C c;
}
