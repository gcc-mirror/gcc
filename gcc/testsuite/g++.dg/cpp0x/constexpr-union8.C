// PR c++/114709
// { dg-do compile { target c++11 } }

struct T1 { int a, b; };
struct T2 { int c; double d; };
union U { T1 t1; T2 t2; };

constexpr int v = U{{1,2}}.t2.*&T2::c; // { dg-error "accessing 'U::t2'" }
