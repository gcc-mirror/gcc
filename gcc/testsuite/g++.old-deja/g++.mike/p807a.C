// { dg-do assemble  }
// prms-id: 807

// See ARM page 275 Section 12.3.2

extern "C" int printf (const char *, ...);
extern "C" void exit(int);

class B;

class A {
public:
	A(B&);			// { dg-message "note" } fn ref in err msg
};

class B {
public:
	operator A();		// { dg-message "candidates" } fn ref in err msg
};

B b;
A a = b;  // { dg-error "ambiguous" } should fail as it is ambigious.
