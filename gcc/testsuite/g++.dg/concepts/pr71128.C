// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename T>
concept bool C() { return true; }
template bool C<int>();  // { dg-error "explicit instantiation of function concept" }

template<typename T>
concept bool D = true;
template bool D<int>;  // { dg-error "explicit instantiation of variable concept" }
