// Build don't link:
// Origin: Theodore Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

double f(double);
typedef double (*M)(double);

class A {
public:
	template <const M n> void g(); 
};

class B: public A {
public:
	void g() { A::g<f>(); }
};
