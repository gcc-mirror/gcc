// PR c++/89404
// { dg-additional-options "-Wno-vla" }

void
foo (int n)
{
  struct {} a[1][n] (+a[0]);  // { dg-error "initialized" }
}
