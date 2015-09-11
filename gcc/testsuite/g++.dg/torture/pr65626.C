// { dg-do compile }

class A {
      virtual unsigned long m_fn1() const;
        virtual int &m_fn2(unsigned long) const;
};
class C : A {
public:
      int &m_fn2(unsigned long) const;
        unsigned long m_fn1() const;
};
class B {
      void m_fn3(const A &, const int &, const C &, int &) const;
};
void B::m_fn3(const A &, const int &, const C &, int &) const {
      C &a(a);
        for (long b = 0; a.m_fn1(); b++)
	      a.m_fn2(0);
}
