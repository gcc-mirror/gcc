// Verify we respect the order of trailing arguments passed to
// __is_constructible.

struct A { };
struct B { };
struct C { C(A, B); };

extern int n[true];
extern int n[ __is_constructible(C, A, B)];
extern int n[!__is_constructible(C, B, A)];
