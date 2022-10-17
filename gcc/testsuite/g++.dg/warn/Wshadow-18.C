// PR c++/104379
// { dg-do compile }
// { dg-options "-Wshadow" }

int x;

template<typename T>
struct S
{
  int i;
  S(int i) { (void) i; }			// { dg-warning "declaration of 'i' shadows a member of 'S<T>'" }
  S(float x) { (void) x; }			// { dg-warning "declaration of 'x' shadows a global declaration" }
  S(int *p) { int a = 1; (void) p; (void) a;
	      { int a = 2; (void) a; } }	// { dg-warning "declaration of 'a' shadows a previous local" }
};

S<int> i(1);
S<long> j(1);
S<int> k(1.0f);
S<long> l(1.0f);
S<int> m(&x);
S<int> n(&x);
