// PR c++/99701
// DR 1512
// { dg-do compile { target c++11 } }

/* Relational comparisons between null pointer constants and pointers are
   ill-formed.  */

decltype(nullptr) foo ();

auto lt = nullptr < nullptr; // { dg-error "ordered comparison" }
auto gt = nullptr > nullptr; // { dg-error "ordered comparison" }
auto le = nullptr <= nullptr; // { dg-error "ordered comparison" }
auto ge = nullptr >= nullptr; // { dg-error "ordered comparison" }
auto eq = nullptr == nullptr;
auto ne = nullptr != nullptr;
auto a1 = nullptr > 0; // { dg-error "ordered comparison" }
auto a2 = 0 < nullptr; // { dg-error "ordered comparison" }
auto a3 = foo () > 0; // { dg-error "ordered comparison" }
auto a4 = 0 < foo (); // { dg-error "ordered comparison" }
auto a5 = 0 <= foo (); // { dg-error "ordered comparison" }
auto a6 = foo () >= 0; // { dg-error "ordered comparison" }
