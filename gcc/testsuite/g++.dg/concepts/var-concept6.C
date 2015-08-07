// { dg-options -std=c++1z }

template <class T>
concept int C = true;		// { dg-error "bool" }
