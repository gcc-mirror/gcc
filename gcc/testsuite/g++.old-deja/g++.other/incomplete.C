// Build don't link:

struct S;

void f(S s) {} // ERROR - incomplete type
