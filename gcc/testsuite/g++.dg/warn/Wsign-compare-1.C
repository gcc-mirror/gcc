// { dg-options "-Wsign-compare" }

extern unsigned u;

template<class F>
int f() { return u > 1; }
