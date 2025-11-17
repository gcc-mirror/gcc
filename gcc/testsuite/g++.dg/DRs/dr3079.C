// DR 3079 - Allow empty-declarations in anonymous unions
// { dg-do compile { target c++11 } }

struct A { union { int x;; } u; };
struct B { union { int x;; }; };
