// { dg-do assemble  }
// GROUPS passed extensions
struct bar { int bit : 1; };

void foo (int *r, bar t)
{
  // doing alignof on a bit-field should be illegal
  __alignof__ (t.bit);// { dg-error "18:invalid application" } .*

  // both of these (a regular ref and an INDIRECT_REF) should work
  __alignof__ (r);
  __alignof__ (*r);
}
