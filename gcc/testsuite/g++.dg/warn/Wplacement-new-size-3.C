// PR c++/71306 - bogus -Wplacement-new with an array element
// { dg-do compile }
// { dg-options "-Wplacement-new" }

void* operator new (__SIZE_TYPE__, void *p) { return p; }

struct S64 { char c [64]; };

S64 s2 [2];
S64* ps2 [2];
S64* ps2_2 [2][2];

void* pv2 [2];

void f ()
{
  char a [2][sizeof (S64)];

  new (a) S64;
  new (a [0]) S64;
  new (a [1]) S64;

  // Verify there is no warning with buffers of sufficient size.
  new (&s2 [0]) S64;
  new (&s2 [1]) S64;

  // ..and no warning with pointers to buffers of unknown size.
  new (ps2 [0]) S64;
  new (ps2 [1]) S64;

  // But a warning when using the ps2_2 array itself as opposed
  // to the pointers it's elements might point to.
  new (ps2_2 [0]) S64;	// { dg-warning "placement new" }
  new (ps2_2 [1]) S64;	// { dg-warning "placement new" }

  // ..and no warning again with pointers to buffers of unknown
  // size.
  new (pv2 [0]) S64;
  new (pv2 [1]) S64;
}
