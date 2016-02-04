// PR c++/59281
// { dg-do compile { target c++11 } }

enum class E : int { prio = 666 };
void f (int) __attribute__((constructor(E::prio))); // { dg-error "integer" }
