// Testcase for wrongful generation of copy constructor.
// Build don't link:

class A { };
class B: virtual private A { };
class D: public B { };		// gets bogus error
