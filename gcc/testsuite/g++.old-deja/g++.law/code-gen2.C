// { dg-do run  }
// GROUPS passed code-generation
// code-gen file
// From: Gunther Seitz <Gunther.Seitz@regent.e-technik.tu-muenchen.dbp.de>
// Date:     Thu, 18 Mar 1993 10:45:29 +0100
// Message-ID: <93Mar18.104538met.1094@regatta.regent.e-technik.tu-muenchen.de>


#include <stdio.h>

class X {

public:
    double x;
    X () { x=3.5; }    // Here we go. This assignment fails because
                       // of X::x being aligned on a doubleword
                       // boundary, not a quadword one.
    };


class A : public virtual X {};       // Only way to produce the
class B : public virtual X {};       // error is to use this
class C : public virtual X {};       // construct of virtual
                                     // base classes.

class Y : public A, public B, public C {};


int main ()
{
        Y y;       // To call the constructor
	printf ("PASS\n");
        }


