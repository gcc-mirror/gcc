// Build don't link: 

struct A1 { struct B { B(); } b; };
struct A2 { struct B { ~B(); } b; };
char xx[] ="../tests/m4.cc:1: warning: return type specification for constructor invalid";


