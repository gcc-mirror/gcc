// PR c++/88115
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-pedantic" }

// Verify we mangle __alignof__ differently from alignof.

#include <cstddef>

template <class T> void f1(decltype(alignof(T))) { }
template <class T> void f2(decltype(alignof(T{}))) { }
template <class T> void f3(decltype(__alignof__(T))) { }
template <class T> void f4(decltype(__alignof__(T{}))) { }

template void f1<int>(std::size_t);
template void f2<int>(std::size_t);
template void f3<int>(std::size_t);
template void f4<int>(std::size_t);

// { dg-final { scan-assembler "_Z2f1IiEvDTatT_E" } }
// { dg-final { scan-assembler "_Z2f2IiEvDTaztlT_EE" } }
// { dg-final { scan-assembler "_Z2f3IiEvDTu11__alignof__T_EE" } }
// { dg-final { scan-assembler "_Z2f4IiEvDTu11__alignof__XtlT_EEEE" } }
