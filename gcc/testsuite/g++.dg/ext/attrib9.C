class __attribute__((unused)) C;
struct __attribute__((unused)) S;
union __attribute__((unused)) U;
enum e {};
enum __attribute__((unused)) e;	// { dg-warning "already defined" }

struct __attribute((unused)) B *p;	//  { dg-warning "attributes" }

template <class T> struct A { };
struct __attribute((unused)) A<int>;	//  { dg-warning "attributes" }
