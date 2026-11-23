// { dg-do compile }
// { dg-options "-O0 -gcodeview" }

// cv_func_htab must not dereference function objects after their bodies have
// been released.

#define F(N) int f##N () { return N; }

F (0)
F (1)
F (2)
F (3)
