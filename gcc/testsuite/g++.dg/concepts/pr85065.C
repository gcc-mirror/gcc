// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<int> concept bool C = true;

C c = 0;  // { dg-error "does not constrain a type" }
