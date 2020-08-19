// PR c++/94553
// { dg-do compile { target c++14 } }

struct C { };
template<typename> int C; // { dg-error "different kind of entity" }
template<typename> int D;
struct D { }; // { dg-error "different kind of entity" }
