// PR c++/33094
// { dg-final { scan-assembler "1BIiE1cE" } }
// { dg-final { scan-assembler-not "globl.*1BIiE1cE" } }
// { dg-final { scan-assembler-not "comdat" } }
// { dg-final { scan-assembler-not "weak" } }
// { dg-final { scan-assembler-not "1AIiE1cE" } }

// Test that B<int>::c is emitted as an internal symbol, and A<int>::c is
// not emitted.

namespace
{
  template <typename T>
  class A
  {
    virtual T f1() { return c; }
    static const T c = 0;
  };

  template <typename T>
  class B
  {
    __attribute__ ((__used__)) static const T c = 0;
  };

  template <typename T> const T B<T>::c;

  template class A<int>;
  template class B<int>;
}
