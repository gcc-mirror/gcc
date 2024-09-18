// PR c++/109859
// { dg-do compile { target c++20 } }

template<typename>
concept A = true;

template<auto = []<A a> {}>
int x;

void g() { (void) x<>; }
