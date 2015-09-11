// PR sanitizer/66452
// { dg-do compile }
// { dg-options "-Wall -fsanitize=undefined" }

class A {
public:
  A(int);
};
class B {
  A m_fn1() const;
};
A B::m_fn1() const {
  for (int i = 0;i;)
    ;
  return 0;
}
