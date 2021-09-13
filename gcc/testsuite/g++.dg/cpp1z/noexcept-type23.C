// PR c++/99374
// { dg-do compile { target c++17 } }

struct S;
struct R;
using F1 = int (S::*)();
using F2 = int (S::*)() noexcept;
using F3 = int (R::*)() noexcept;
using T = S;
using F4 = int (T::*)() noexcept;
F1 f21 = F2();
F1 f41 = F4();
F2 f12 = F1(); // { dg-error "cannot convert" }
F1 f31 = F3(); // { dg-error "cannot convert" }
