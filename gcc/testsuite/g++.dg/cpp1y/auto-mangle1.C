// Mangling for decltype(auto)
// { dg-options "-std=c++1y" }

void f();

// { dg-final { scan-assembler "_Z2g1IiEDcv" } }
template <class T> decltype(auto) g1() { return &f; }
template decltype(auto) g1<int>();

// { dg-final { scan-assembler "_Z2g2IiEDav" } }
template <class T> auto g2() { return &f; }
template auto g2<int>();
