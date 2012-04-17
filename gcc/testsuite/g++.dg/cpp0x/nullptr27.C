// PR c++/52706
// { dg-options "-std=c++11 -fabi-version=0" }
// { dg-final { scan-assembler "_Z1fIDnLDn0EEiT_" } }

template<class T, decltype(nullptr) = nullptr>
int f(T);

int i2 = f(nullptr); // 17
