static inline int foo (int x) { return x + 1; }
__attribute__ ((__optimize__ (0))) int bar (void) { return foo (100); }
