// PR c++/96876
// { dg-do compile { target c++17 } }

struct B {
protected:
    ~B() {}			// { dg-message "" }
};

struct A { };
struct C1: B { int n; };
struct C2: A, B { int n; };

A af ();
int f();

void g() {
  C1 c1{ {}, f()};		// { dg-error "protected" }
  C2 c2{ af(), {}, f()};	// { dg-error "protected" }
}
