// { dg-do assemble  }
// GROUPS passed parsing
// From: Jason Merrill <jason@cygnus.com>
// Date:     Fri, 13 Aug 93 12:49:11 PDT
// Subject:  2.4.5 won't compile array of pointers to functions returning T
// Message-ID: <9308131949.AA26348@cygnus.com>
// From: "Robert M. Keller" <keller@jarthur.Claremont.EDU>
// Subject: g++ bug
// Date: Fri, 13 Aug 93 10:09:27 PDT

/* Testing declaration of "array of pointers to functions returning T" */

typedef int T;

T foo()
{ return 10; }

T bar()
{ return 20; }

T baz()
{ return 30; }

int main()
{
T (*apfrt[10])();

apfrt[0] = foo;
apfrt[1] = bar;
apfrt[2] = baz;

}
