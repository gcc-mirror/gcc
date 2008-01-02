// PR c++/33493

template<int> void foo() { delete 0 ? 1 : 0; } // { dg-error "delete 0" }
