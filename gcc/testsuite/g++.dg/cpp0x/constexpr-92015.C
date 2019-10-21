// PR c++/92015
// { dg-do compile { target c++11 } }

struct S1 { char c[6] {'h', 'e', 'l', 'l', 'o', 0}; };
struct S2 { char c[6] = "hello"; };
static_assert (S1{}.c[0] == 'h', "");
static_assert (S2{}.c[0] == 'h', "");
