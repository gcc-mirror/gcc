// { dg-do assemble  }
// { dg-options "-Wformat" }
// Test that attributes are really applied to function declarations under
// various conditions.
// Contributed by Jason Merrill (jason@cygnus.com)

#define PF __attribute__ ((format (printf, 1, 2)))

struct A {
  static PF void f (char *, ...);
  friend PF void g (char *, ...);
  static void test ();
};

void PF h (char *, ...);
void PF k (char *, ...) { }

void A::test ()
{
  f ("%f", 42);			// { dg-warning "" } 
  g ("%f", 42);			// { dg-warning "" } 
  h ("%f", 42);			// { dg-warning "" } 
  k ("%f", 42);			// { dg-warning "" } 
}
