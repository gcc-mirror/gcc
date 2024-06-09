// PR c++/96997
// { dg-additional-options "-g -fdump-tree-gimple-lineno" }

struct A { A(); ~A(); };
void f(const A& = A());
int main() { f(); }

// The destructor call for the A temporary should not have the location of the
// f declaration.
// { dg-final { scan-tree-dump-not ".C:5" "gimple" } }
