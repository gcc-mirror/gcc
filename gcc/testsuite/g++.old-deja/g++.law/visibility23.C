// Build don't link: 
// GROUPS passed visibility
// visibility file
// From: Robert Carden <carden@thoth.ics.uci.edu>
// Date:     Thu, 12 Aug 1993 13:47:11 -0700
// Subject:  bug 8/12/93 -- #4
// Message-ID: <9308121347.aa26185@Paris.ics.uci.edu>
//
// 4.cc
//
#include <stream.h>

class A {
        int x;
public:
        void f(int);
        void f(float);
        void g(void *);
};


class B : public A {
private:
        A::f;
        A::g;// ERROR - .* , XFAIL *-*-*
};
