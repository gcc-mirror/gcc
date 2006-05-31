// PR c++/27808

template<typename T> friend void T::foo; // { dg-error "friend|invalid" }
