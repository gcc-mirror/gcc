// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: cmwang@iis.sinica.edu.tw (Chien-Min Wang)
// Date:     Fri, 6 Aug 93 19:42:31 CST
// Subject:  A bug in g++ 2.4.5
// Message-ID: <9308061142.AA08533@iiserv>
struct T1 { int i; };

struct T2 { int j; }; // { dg-error "" } private

struct T3 : public T1, private T2 {
} x;

int main ()
{
    x.i = 1;
    x.j = 2;    // error: x.j is private// { dg-error "" } .*
    return 0;
}
