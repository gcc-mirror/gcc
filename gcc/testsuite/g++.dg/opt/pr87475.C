// PR rtl-optimization/87475
// { dg-do compile { target freorder } }
// { dg-options "-O2 -freorder-blocks-and-partition -fmodulo-sched" }

struct A { A (); ~A (); };
int foo (A, A);
void bar (bool x) { x ? foo (A (), A ()) : 0; }
