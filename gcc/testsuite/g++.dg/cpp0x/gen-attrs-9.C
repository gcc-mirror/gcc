// { dg-do compile { target c++11 } }

class C;
struct S;
union U;
enum e {};
enum [[gnu::unused]] e;	// { dg-warning "already defined" }

struct [[gnu::unused]] B *p;	//  { dg-warning "attributes" }

template <class T> struct A { };
struct [[gnu::unused]] A<int>;	//  { dg-warning "attributes" }
