// PR c++/99565
// { dg-do compile }
// { dg-options "-Wduplicated-branches" }

int a;

void
foo (bool x)
{
  x ? ++a : ++a;	// { dg-warning "this condition has identical branches" }
}
