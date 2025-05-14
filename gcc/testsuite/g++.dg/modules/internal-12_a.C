// PR c++/119551
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }
// Test that emitting variables referencing TU-local entities
// builds and runs correctly.

export module M;

static int tu_local_var = 5;
static int* tu_local_func() { return &tu_local_var; }

export int* a = &tu_local_var;
export inline int* b = tu_local_func();
