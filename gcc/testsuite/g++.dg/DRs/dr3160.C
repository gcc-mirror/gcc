// CWG 3160
// { dg-do compile { target c++17 } }

typedef int t1, t2;
template<class> struct u1;
struct A {
  A(t1);         // OK, declares constructor having unnamed parameter of type `int`, not a data member named `t1`
  typedef A(t2); // OK, declares `t2` as an alias for `A`
  A(u1);         // { dg-error "placeholder" } for deduced class type cannot appear in this context (9.2.9.8 [dcl.type.class.deduct])
};
