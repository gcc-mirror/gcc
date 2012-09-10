// PR c++/54538
// { dg-do compile { target c++11 } }

template <class T>
struct A
{
  // { dg-final { scan-assembler "_ZNK1AIcE1pMUlvE_cvPFvvEEv" } }
  // { dg-final { scan-assembler "_ZNK1AIiE1pMUlvE_cvPFvvEEv" } }
  void (*p)() = []{};
};

A<int> a1;
A<char> a2;
