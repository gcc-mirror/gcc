// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept bool C() { return true; } // { dg-error "the .bool. keyword" }
template bool C<int>();  // { dg-error "explicit instantiation of function concept" }

template<typename T>
concept bool D = true;	// { dg-error "the .bool. keyword" }
template bool D<int>;  // { dg-error "explicit instantiation of variable concept" }
