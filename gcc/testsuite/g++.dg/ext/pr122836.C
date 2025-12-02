// PR c++/122836
// { dg-do compile }
// { dg-options "" }

struct V { __attribute__ ((__vector_size__ (2 * sizeof (float)))) float v[2]; };

V
foo ()
{
  return (V) { { .v = { 0, 0 } } };	// { dg-error "name 'v' used in a GNU-style designated initializer for an array" }
}
