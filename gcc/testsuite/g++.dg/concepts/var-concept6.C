// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template <class T>
concept int C = true;		// { dg-error "bool" }
