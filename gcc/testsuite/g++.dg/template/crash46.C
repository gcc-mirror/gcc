// PR c++/27102

template <class T>
void T::foo() {} // { dg-error "invalid" }

