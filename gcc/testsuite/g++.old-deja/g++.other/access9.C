// Test that g++ allows friends to use private types in their declarations.
// Build don't link:

class A {
  typedef int I;
  friend I f (I);
};

A::I f (A::I);
A::I f (A::I) { return 0; }
