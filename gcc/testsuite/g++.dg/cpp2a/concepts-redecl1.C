// PR c++/94553
// { dg-do compile { target c++20 } }

struct E { };
template<typename> concept E = false; // { dg-error "different kind of entity" }
template<typename> concept F = false;
struct F { }; // { dg-error "different kind of entity" }
