// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T>
concept int C = true;		// { dg-error "concept definition syntax|variable concepts are no longer supported" }
