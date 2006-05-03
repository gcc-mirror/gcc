// PR c++/27102

template <typename T>
void T::foo; // { dg-error "invalid" }
