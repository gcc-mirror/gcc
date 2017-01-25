// PR c++/72707
// { dg-do compile }

void
foo (double x)
{
  union { int x; };	// { dg-error "shadows a parameter" }
  x = 0;
}
