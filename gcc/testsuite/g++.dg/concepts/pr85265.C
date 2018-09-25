// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template<typename> concept bool C = true;

C{} void foo();  // { dg-error "expected identifier" }
