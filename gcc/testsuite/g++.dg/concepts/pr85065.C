// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<int> concept C = true;

C c = 0;  // { dg-error "does not constrain a type" }
