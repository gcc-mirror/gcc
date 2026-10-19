// { dg-do compile { target c++14 } }
// { dg-options "-O2 -fdump-tree-optimized" }

// The elements of t after the first share one initializer, { 0, 3 }, under
// a RANGE_EXPR: the two elements of its constructor cover all eight of the
// array, so no load reads a zero from an element left out.

struct S { int a; int b = 3; };
static const S t[8] = { { 1, 2 } };

void link_error ();

int
foo (unsigned i)
{
  int v = t[i].b;
  if (v != 2 && v != 3)
    link_error ();
  return v;
}

// { dg-final { scan-tree-dump-not "link_error" "optimized" } }
