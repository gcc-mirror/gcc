// { dg-additional-options "-fmodules-ts" }

import M;

A<int> a;
struct S {};  // { dg-error "conflicts with import" }
template <typename> struct T {};  // { dg-error "conflicts with import" }

B<int> c;
void f() {}  // { dg-error "conflicts with import" }
template <typename> void g() {}  // { dg-error "conflicts with import" }
