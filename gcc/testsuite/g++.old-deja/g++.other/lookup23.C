// { dg-do assemble  }
// Test for proper handling of type lookup if base class has field with the
// same name as the containing class.

struct a { int a; };
struct b : a {};

b x;

void foo ()
{
  x.a = 22;
}
