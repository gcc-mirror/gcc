// GROUPS passed constructors
// ctor file
// Message-Id: <9306021533.AA14347@icepick.jts.com>
// From: roland@jts.com (Roland Knight )
// Subject: gcc 2.4.1 bug
// Date:   Wed, 2 Jun 1993 11:33:34 -0400

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char stuff[50];
char *p = stuff;

class A {
public:
    A() { *p++ = 'A';}
};

class B {
public:
    B() { *p++ = 'B'; }
};

class C : public A, public B {
public:
    C() : B(), A() { *p++ = 'C'; }
};

class D : public A, public B {
public:
    D() : B() { *p++ = 'D'; }
};

class E : public A, public B {
public:
    E() { *p++ = 'E'; }
};


int main() {
    C c;
    D d;
    E e;
    if (strncmp ("ABCABDABE", stuff, 9))
	{ printf ("FAIL\n"); return 1; }
    else
	printf ("PASS\n");
}
