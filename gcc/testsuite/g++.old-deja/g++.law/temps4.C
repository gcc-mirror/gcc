// { dg-do run  }
// { dg-options "-felide-constructors" }
// GROUPS passed temps
// temps file
// Message-Id: <9311102043.AA22871@ses.com>
// From: jamshid@ses.com (Jamshid Afshar)
// Subject: elide-constructors (aka return value optimization)
// Date: Wed, 10 Nov 93 14:43:54 CST

#include <stdio.h>
#include <stdlib.h>

class X {
    int i;
  public:
    X();
    X(const X&);
    X(int);
    ~X();
};

int did_it = 0;

X::X() { ; }
X::X(const X&) { did_it = 1; }
X::X(int) { ; }
X::~X() { ; }

X foo() {
    X x(1);
    return x;
}

int
main() {
    X x = foo();
    if (did_it)
	abort ();
    else
	printf ("PASS\n");

    return 0;
}
