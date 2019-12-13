// PR c++/91979
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=13" }
// { dg-final { scan-assembler "_Z1fIDnLDn0EEiT_" } }

template<class T, decltype(nullptr) = nullptr>
int f(T);

int i2 = f(nullptr);
