// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: Robert Carden <carden@thoth.ics.uci.edu>
// Date:     Thu, 12 Aug 1993 13:48:05 -0700
// Subject:  bug 8/12/93 -- #5
// Message-ID: <9308121348.aa26256@Paris.ics.uci.edu>

// 5.cc
#include <iostream>

class A {
        int x;
public:
        void f(int);
        void f(float);
        void g(void *);
};

class B : private A {
protected:
        A::f; // { dg-warning "deprecated" }
public:
        A::g; // { dg-warning "deprecated" }
};
