// { dg-do compile }
// { dg-options --std=c++0x }

struct A {
	int i, j;
	A () : A(0), j(0) { } // { dg-error "constructor delegation" }
	A (int _i) : i(_i) { }
};

struct B {
	int i, j;
	B () : i(0), B(0) { } // { dg-error "constructor delegation" }
	B (int _j) : j(_j) { }

};

struct C {};

struct D : public C {
	D () : C() { }
	D (float) : D(), C() { } // { dg-error "constructor delegation" }
	D (float, float): C(), D() { } // { dg-error "constructor delegation" }
};
