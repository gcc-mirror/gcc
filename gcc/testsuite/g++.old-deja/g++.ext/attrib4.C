// { dg-do assemble  }
// { dg-options "-Wformat" }
// Test that attributes are really applied to function declarations under
// various conditions.
// Contributed by Jason Merrill (jason@cygnus.com)

#define PF __attribute__ ((format (printf, 1, 2)))

struct A {
  static PF void f (const char *, ...);
  static PF void g (const char *, ...) { }
  static void test ();
};

void PF h (const char *, ...);
void PF k (const char *, ...) { }

void A::test ()
{
  f ("%f", 42);			// { dg-warning "argument 2" }
  g ("%f", 42);			// { dg-warning "argument 2" }
  h ("%f", 42);			// { dg-warning "argument 2" }
  k ("%f", 42);			// { dg-warning "argument 2" }
}
