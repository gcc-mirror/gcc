// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept bool C() { return true; } // { dg-error "the .bool. keyword|function concepts" }
template bool C<int>();  // { dg-error "template function|not a function template|expected" }

template<typename T>
concept bool D = true;	// { dg-error "the .bool. keyword|variable concepts are no longer supported" }
template bool D<int>;  // { dg-error "not a template function|expected" }
