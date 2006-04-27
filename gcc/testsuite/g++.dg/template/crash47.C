// PR c++/27102

template<typename T> void T::X::foo() {} // { dg-error "invalid" }
