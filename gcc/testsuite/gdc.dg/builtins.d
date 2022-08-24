// { dg-do compile }
// { dg-options "-fdump-tree-original" }

// { dg-final { scan-tree-dump " __builtin_sqrt " "original" } }
extern(C) double sqrt(double);
double test_sqrt(double a) { return sqrt(a); }

// { dg-final { scan-tree-dump-not " __builtin_tan " "original" } }
pragma(mangle, "tan")
extern(C) double libc_tan(double);
double test_tan(double a) { return libc_tan(a); }

// { dg-final { scan-tree-dump " __builtin_malloc " "original" } }
// { dg-final { scan-assembler "mangle_override" } }
pragma(mangle, "mangle_override")
extern(C) void *malloc(size_t);
void* test_malloc(size_t a) { return malloc(a); }
