// { dg-do run  }
// GROUPS passed temps
// temps file
// Message-Id: <9308231535.AA19432@geant.cenatls.cena.dgac.fr>
// From: chatty@geant.cenatls.cena.dgac.fr (Stephane CHATTY)
// Subject: g++ 2.4.5 does not destroy temporaries
// Date: Mon, 23 Aug 93 17:35:34 +0200

#include <stdio.h>

class A {
public:
        int a;
        A (int i) : a (i) { ;}
        A (const A& aa) : a (aa.a) { ;}
        ~A () { printf ("PASS\n");; }
};

A
foo ()
{
        return A (10);
}

int main ()
{
        int x = foo ().a;
}

