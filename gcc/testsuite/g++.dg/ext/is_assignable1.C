// PR c++/109997

struct S;
bool b = __is_assignable(int, S); // { dg-error "incomplete" }
