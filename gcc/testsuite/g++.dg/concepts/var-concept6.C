// { dg-options "-std=c++1z -fconcepts" }

template <class T>
concept int C = true;		// { dg-error "bool" }
