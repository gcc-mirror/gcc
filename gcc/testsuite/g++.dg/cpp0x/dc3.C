// { dg-do compile { target c++11 } }

struct X {};

struct B {
	int i;
	B (int _i) : i(_i) { }
	~B () { i = 0; }
};

template <typename T>
struct A : public B {
	A () : B(-1) { }
	~A () { }
};

template <typename T>
struct A<T*> : public B {
	A () : B(-1) { }
	A (int i) : A() { }
	A (double b) : A(static_cast<int>(b)) { }
	A (double b, double b2) : A(b2) { }
	~A () { }
};

void f_A () { A<X*> a(2.0, 3.0); }

struct C {
	C () { }
	virtual ~C() { }
	virtual int f () = 0;
};

template <typename T>
struct D : public C {
	int i;
	D (int _i) : C(), i(_i) { }
};

template <>
struct D<X> : public C {
	int i;
	D (int _i) : C(), i(_i) { }
	D () : D(-1) { }
	virtual ~D() { }
	virtual int f () { }
};

void f_D () { D<X>* d = new D<X>(); }

template <typename T>
struct E {
};

template <>
struct E<int> {
	int i;
	E () : E(0) { }
	E (int _i) : i(_i) { }
};

void f_E () { E<int> e; }
