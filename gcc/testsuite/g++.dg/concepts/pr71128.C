// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template<typename T>
concept bool C() { return true; }
template bool C<int>();  // { dg-error "explicit instantiation of function concept" }

template<typename T>
concept bool D = true;
template bool D<int>;  // { dg-error "explicit instantiation of variable concept" }
