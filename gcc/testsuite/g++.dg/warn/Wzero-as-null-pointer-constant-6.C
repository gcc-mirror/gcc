// PR c++/53301
// { dg-options "-Wzero-as-null-pointer-constant" }

class x { public: x(int v) {} };

void foo(const x& = 0);
