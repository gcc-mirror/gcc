// PR tree-optimization/69483
// { dg-do compile }

struct T { struct S *a; };
struct S b; // { dg-error "aggregate 'S b' has incomplete type and cannot be defined" }
struct T c = { &b };
