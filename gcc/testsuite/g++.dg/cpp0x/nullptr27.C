// PR c++/52706
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0" }
// { dg-final { scan-assembler "_Z1fIDnLDn0EEiT_" } }

template<class T, decltype(nullptr) = nullptr>
int f(T);

int i2 = f(nullptr); // 17
