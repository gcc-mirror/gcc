// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template<int> concept bool C = true;

C c = 0;  // { dg-error "invalid reference to concept" }
