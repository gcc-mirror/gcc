// Build don't link: 
// GROUPS passed vbase
// vbase file
// From: pino@hubble.eecs.berkeley.edu (Jose Luis Pino)
// Date:     28 Jul 1994 05:17:39 GMT
// Subject:  g++ 2.6 bug: virtual base class & protected methods
// Message-ID: <317f1j$o9c@agate.berkeley.edu>


#include <iostream>

class a {
protected:
	virtual void foo() { std::cout << "Class A\n";}
};

class b : public virtual a {};

class c : public b {
public:
	void bar() { b::foo();}
};

int main() {
	c test;
	test.bar();
}


