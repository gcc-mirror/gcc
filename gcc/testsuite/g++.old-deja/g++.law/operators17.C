// Build don't link: 
// Special g++ Options: -Wall -pedantic-errors
// GROUPS passed operators
// copy file
// From: gfm@mencon.mencon.oz.au (Graham Menhennitt)
// Date:     Thu, 29 Apr 93 20:53:07 EST
// Subject:  4 bugs in g++ 2.3.3
// Message-ID: <9304291053.AA00090@mencon>

        struct A {
                A& operator = (const A& a) {}// ERROR - XFAIL
        };
