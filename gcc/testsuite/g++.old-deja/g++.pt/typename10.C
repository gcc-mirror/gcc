// Build don't link:

struct S {
  typedef int I;
};

void f(typename S::I); // ERROR - using typename outside of template
