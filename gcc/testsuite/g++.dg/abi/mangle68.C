// PR c++/89356
// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

template<typename T>
auto fn () -> decltype(unsigned{2u} + (T)3) { return 42; }

// { dg-final { scan-assembler "_Z2fnIiEDTpltljLj2EEcvT_Li3EEv" } }
template auto fn<int>() -> decltype(unsigned{2u} + (int)3);
