// { dg-do assemble  }

struct B { struct A { A(); int a; } aa; };
B::A::A () { a = 37; }
char* xx[] = {
"../tests/m6.cc:1: warning: return type specification for constructor invalid",
"../tests/m6.cc:2: semicolon missing after declaration of `A'",
"../tests/m6.cc:2: warning: empty declaration",
"../tests/m6.cc: In function int A ():",
"../tests/m6.cc:2: `a' undeclared (first use this function)",
"../tests/m6.cc:2: (Each undeclared identifier is reported only once",
"../tests/m6.cc:2: for each function it appears in.)",
"../tests/m6.cc:2: warning: control reaches end of non-void function" };

