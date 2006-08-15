//PR c++/28594

template<void, int> struct A; // { dg-error "not a valid type" }
