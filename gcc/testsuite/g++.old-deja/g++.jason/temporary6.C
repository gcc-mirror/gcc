// { dg-do assemble  }
// Bug: the temporary from the default parameter to f2 is reused.

struct A {};
int f2 (int i, const A& ar = A());
void f (int i, int j = f2(1));
void g () { f (1); }
void h () { f (1); }
