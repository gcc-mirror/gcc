// Build don't link: 

struct B { struct A { A(); int a; }; A aa; };
B::A::A () { a = 37; }
char *xx[]= {"/*",
"../tests/m7.cc:1: warning: return type specification for constructor invalid",
"../tests/m7.cc: In function struct A A ():",
"../tests/m7.cc:2: `a' undeclared (first use this function)",
"../tests/m7.cc:2: (Each undeclared identifier is reported only once",
"../tests/m7.cc:2: for each function it appears in.)",
"../tests/m7.cc:2: warning: control reaches end of non-void function",
	       "*/" };

