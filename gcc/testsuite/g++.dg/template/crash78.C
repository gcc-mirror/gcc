// PR c++/35077

template<typename=int struct A __attribute((aligned(4))); // { dg-error "declaration|expected" }
