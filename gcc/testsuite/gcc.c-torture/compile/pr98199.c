/* PR tree-optimization/98199 */
/* { dg-require-effective-target int32plus } */

struct A { long a; short d; int c, f, e, g; };
struct B { int a, i; short j; struct A k; signed : 20; int e, g; } __attribute__((packed));
struct C { short a; unsigned i, k; struct B d; const int : 30; signed e : 20; signed : 18; };
const struct C l = { 1, 6, 0, {}, 0 };
int foo (void) { return l.e || 0; }
