// { dg-do assemble  }
// Test that we properly diagnose an attempt to use an anonymous class
// in declaring an external function.

typedef const struct { int i; } T; // { dg-error "" } referenced below
void f (T* t);			// { dg-error "" } uses unnamed type

int main()
{
  f(0);
}
