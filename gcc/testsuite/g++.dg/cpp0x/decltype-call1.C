// PR c++/52748
// N3276
// { dg-do compile { target c++11 } }

struct A;			// { dg-error "forward declaration" }
A f();

decltype(f()) g1();		 // OK
decltype(((f()))) g2b();	 // OK
decltype(42,f()) g3();		 // OK
decltype(42,45,f()) g3b();	 // OK
decltype(42,45,(f())) g3c();	 // OK
decltype(42,((45,(f())))) g3c(); // OK

decltype(f(),42) g4();		 // { dg-error "" }
decltype(45,f(),42) g4b();	 // { dg-error "" }

class B
{
  ~B();				// { dg-error "private" }
public:
  int i;
  void operator[](int);
};
B h();

void i(const B&);

decltype(h()) g5a();		// OK
decltype(h().i) g5();		// { dg-error "" }
decltype(h()[0]) g6();		// { dg-error "" }
decltype(i(h())) g7();		// { dg-error "" }
