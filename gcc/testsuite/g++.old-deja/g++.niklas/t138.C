// { dg-do assemble  }
// GROUPS passed niklas hiding
struct A;
void f (A*);
A* A;
void g () { f (A); }
