// N5008 [expr.prim.id.unqual]/p7
// Otherwise, if the unqualified-id appears in the predicate of a contract assertion C (6.10) and the entity is
// (7.1) — a variable declared outside of C of object type T,
// (7.2) — a variable or template parameter declared outside of C of type “reference to T”, or
// (7.3) — a structured binding of type T whose corresponding variable is declared outside of C,
// then the type of the expression is const T
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=enforce -O" }

struct S{
  S(){};
  S(const S&){}
  ~S(){};
  int x = 0;
};

int i = 3;
int *pi = &i;
int *pi2 = &i;


void f1() pre(i++) {} // { dg-error "increment of read-only location" }
void f2() pre((*pi)++) {};
void f3() pre(pi2++) {} // { dg-error "increment of read-only location" }

int a[2] = {1, 2};
auto [x, y] = a;
auto& [xr, yr] = a;

void f4() pre(x++) {} // { dg-error "increment of read-only location" }
void f5() pre(xr++) {}; // { dg-error "increment of read-only location" }

void f7(int x) pre([](int& i)
	      {
		return true;
	      }(x)){} // { dg-error "discards qualifiers" }
// { dg-error {no match for call to} "" { target *-*-* } .-1 }


int g = 0;
struct X { bool m(); };
struct Y {
  int z = 0;

  void f(int i)  pre (++g); // { dg-error "increment of read-only location" }
  void f2(int i) pre (++i); // { dg-error "increment of read-only location" }
  void f3(int* p) pre (++(*p)); // OK
  void f4(int& r) pre (++r); // { dg-error "increment of read-only location" }
  void f5(X x) pre (x.m()); // { dg-error " argument discards qualifiers" }
  void f6(X* px) pre (px->m()); // OK

  template <int N, int& R, int* P>
  void tf1() pre(++N); 	// { dg-error "increment of read-only location" }
  // { dg-error {lvalue required as increment operand} "" { target *-*-* } .-1 }

  template <int N, int& R, int* P>
  void tf2() pre(++R);	// { dg-error "increment of read-only location" }

  template <int N, int& R, int* P>
  void tf3() pre(++(*P)); // OK

  int h1() post(r : ++r); // { dg-error "increment of read-only" }

  int& k() post(r : ++r); // { dg-error "increment of read-only" }
};
