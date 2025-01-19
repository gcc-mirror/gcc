// PR c++/117463
// { dg-do "compile" { target c++20 } }

struct S {};
void *operator new[] (unsigned long, // { dg-bogus "first parameter" "" { xfail *-*-* } }
		      void void *volatile p); // { dg-error "two or more" }
S *fun(void *p) {
  return new(p) S[10];
}

void *operator new (decltype(sizeof(0)), // { dg-bogus "first parameter" "" { xfail *-*-* } }
		    void void * p); // { dg-error "two or more" }
void *p;
auto t = new(p) int;
