// { dg-do compile { target c++17 } }
// { dg-additional-options "-fconcepts" }

template<T> concept C = true;  // { dg-error "has not been declared" }
