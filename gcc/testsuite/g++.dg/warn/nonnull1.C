// Test that "nonnull" attribute works for C++.
// Origin: Joseph Myers <jsm@polyomino.org.uk>
// { dg-do compile }
// { dg-options "-Wall" }

// The "nonnull" attribute is thoroughly tested for C, so here we
// simply test that it works at all, as at one point the relevant
// checking code was only called for C.

extern void f (char *) __attribute__((nonnull));

void
g ()
{
  f (0); // { dg-warning "null" "null argument" }
}
