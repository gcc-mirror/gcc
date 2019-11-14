// PR target/92038
// { dg-do compile }
// { dg-options "-O2 -flifetime-dse=2" }

struct A { A (int); int a; };
struct B { B () : b(0) {} A b; };
struct C { C () : c () {} bool c; B d; };
void foo () { C d; }
