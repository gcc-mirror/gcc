// PR c++/102617
// P2360R0: Extend init-statement to allow alias-declaration
// { dg-do compile { target c++23 } }
// Test invalid use.

int v[10];
namespace N { using X = int; }

void
g ()
{
  for (using N::X; false;) // { dg-error "expected" }
    ;
  for (using N::X; int e : v) // { dg-error "expected" }
    (void) e;
  for (using T = int; using U = int; int e : v) // { dg-error "" }
    ;
  if (using N::X; false) // { dg-error "expected" }
    {}
  switch (using N::X; 0) // { dg-error "expected" }
    ;
 if (using T = int;) // { dg-error "expected" }
  {
  }
}
