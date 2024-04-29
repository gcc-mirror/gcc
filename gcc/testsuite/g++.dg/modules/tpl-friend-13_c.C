// { dg-additional-options "-fmodules-ts" }

import M;

struct S {};  // { dg-error "conflicts with import" }
template <typename> struct T {};  // { dg-message "previously declared" }
A<int> a;  // { dg-message "required from here" }

void f() {}  // { dg-message "previously declared" }
template <typename> void g() {}  // { dg-message "previously declared" }
B<int> b;  // { dg-message "required from here" }

// { dg-error "conflicting declaration" "" { target *-*-* } 0 }
