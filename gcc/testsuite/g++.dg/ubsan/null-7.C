// PR sanitizer/70342
// { dg-do compile }
// { dg-options "-fsanitize=null" }

class A {};
class B {
public:
  B(A);
};
class C {
public:
  C operator<<(B);
};
class D {
  D(const int &);
  C m_blackList;
};
D::D(const int &) {
  m_blackList << A() << A() << A() << A() << A() << A() << A() << A() << A()
              << A() << A() << A() << A() << A() << A() << A() << A() << A()
              << A() << A() << A() << A() << A() << A() << A() << A() << A()
              << A() << A() << A() << A() << A() << A() << A() << A() << A()
              << A() << A() << A() << A() << A() << A() << A() << A() << A();
}
