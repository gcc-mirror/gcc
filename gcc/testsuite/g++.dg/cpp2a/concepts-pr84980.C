// { dg-do compile { target c++17_only } }
// { dg-additional-options "-fconcepts-ts" }

template<T> concept bool C = true;  // { dg-error "has not been declared" }
