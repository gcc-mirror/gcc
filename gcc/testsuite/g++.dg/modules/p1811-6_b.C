// { dg-additional-options "-fmodules" }

import M;

auto imported_a = make_a ();
auto imported_b = make_b ();

template <class T> using A = T; // { dg-error "conflicting declaration" }
template <class T> int B; // { dg-error "conflicting declaration" }
