// Test that we can represent static_casts in template arg lists.
// Build don't link:

template <int I> struct A { };

template <class T> struct B {
  A<static_cast<T>(3.14)> a;
};
