// { dg-do assemble  }
// { dg-options "-ansi -Wall -pedantic" }
// GROUPS passed ARM-compliance
// arm file
// From: Olaf.Weber@cwi.nl
// Date:     Fri, 2 Dec 1994 09:14:25 +0100
// Subject:  Omitting & when obtaining a pointer to member function.
// Message-ID: <9412020814.AA00604=olaf@havik.cwi.nl>

struct C {
        void foo();
};

void (C::*pfm)() = C::foo;// { dg-error "" } .*
