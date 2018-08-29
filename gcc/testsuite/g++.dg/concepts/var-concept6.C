// { dg-options "-std=c++17 -fconcepts" }

template <class T>
concept int C = true;		// { dg-error "bool" }
