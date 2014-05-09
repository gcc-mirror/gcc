// CWG 5

struct C { };
C c;
struct A {
  A(const A&);
  A(const C&);
};
const volatile A a = c;    // Okay
