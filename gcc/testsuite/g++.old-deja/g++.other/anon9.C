// Test that we properly diagnose an attempt to use an anonymous class
// in declaring an external function.

typedef const struct { int i; } T; // ERROR - referenced below
void f (T* t);			// ERROR - uses unnamed type
