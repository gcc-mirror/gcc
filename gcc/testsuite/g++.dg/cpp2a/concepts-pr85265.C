// PR c++/85265
// { dg-do compile { target c++17_only } }
// { dg-additional-options "-fconcepts" }

template<typename> concept bool C = true;

C{} void foo();  // { dg-error "expected identifier" }
