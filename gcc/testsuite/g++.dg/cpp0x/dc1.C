// { dg-do compile { target c++11 } }

struct B {
	int i;
	B (int _i) : i(_i) { }
	~B () { i = 0; }
};

struct A : public B {
	A () : B(-1) { }
	A (int i) : A() { }
	A (double b) : A(static_cast<int>(b)) { }
	A (double b, double b2) : A(b2) { }
	~A () { }
};

void f_A () { A a(2.0, 3.0); }

struct C {
	C () { }
	virtual ~C() { }
	virtual int f () = 0;
};

struct D : public C {
	int i;
	D (int _i) : C(), i(_i) { }
	D () : D(-1) { }
	virtual ~D() { }
	virtual int f () { return 0; }
};

void f_D () { C* c = new D(); }

template <typename T>
struct E {
	T t;
	E () : E(T()) { }
	E (T _t) : t(_t) { }
};

void f_E () { E<int> e; }
