// Test that defining a static member of private type with the () syntax works.
// Build don't link:

class A {
  private:
    struct B { B(int) {} };
    static B b;
};
A::B A::b (1);
