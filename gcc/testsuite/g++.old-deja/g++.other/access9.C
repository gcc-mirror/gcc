// { dg-do assemble  }
// Test that g++ allows friends to use private types in their declarations.

class A {
  typedef int I;
  friend I f (I);
};

A::I f (A::I);
A::I f (A::I) { return 0; }
