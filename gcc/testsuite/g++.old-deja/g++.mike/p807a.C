// { dg-do assemble  }
// prms-id: 807

// See ARM page 275 Section 12.3.2

extern "C" int printf (const char *, ...);
extern "C" void exit(int);

class B;

class A {
public:
	A(B&);			// { dg-error "" } fn ref in err msg
};

class B {
public:
	operator A();		// { dg-error "" } fn ref in err msg
};

B b;
A a = b;  // { dg-error "" } should fail as it is ambigious.
