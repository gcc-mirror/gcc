// GROUPS passed operators
// copy file
// From: gfm@mencon.mencon.oz.au (Graham Menhennitt)
// Date:     Thu, 29 Apr 93 20:53:07 EST
// Subject:  4 bugs in g++ 2.3.3
// Message-ID: <9304291053.AA00090@mencon>

#include <stdio.h>

int pass = 0;
struct A {
        A(void) {}
        A(const A& a) { ; }
        A& operator = (const A& a) { pass = 1; return *this; }
};

struct B {
        B(const A& aa) { B::a = aa; }
        A a;
};

int main(void)
{
        B b = A();
	if (pass)
		printf ("PASS\n");
	else
		printf ("FAIL\n");
}
