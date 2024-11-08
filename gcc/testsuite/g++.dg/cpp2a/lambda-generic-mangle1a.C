// PR c++/109422
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fabi-version=18" }

struct C {
  template<typename T>
  void f(decltype([](T, auto) { return 0; })) {}
};
void g() { C().f<int>({}); }
// { dg-final { scan-assembler "_ZN1C1fIiEEvDTLNS_UlT_T_E_EEE" } }
