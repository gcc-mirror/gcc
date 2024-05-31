// PR c++/85133
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

/* The error with "concept bool" used to be "variable concept has no
    initializer" which is much better.  Let's at least test that we
    do not crash.  */

template<typename> concept C; // { dg-error "expected" }

template<C...> struct A {}; // { dg-error "declared" }

A<int> a; // { dg-error "" }
