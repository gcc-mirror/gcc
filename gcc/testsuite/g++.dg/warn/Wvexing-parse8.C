// PR c++/97762
// { dg-do compile }

void
g ()
{
  long a(); // { dg-warning "empty parentheses" }
  signed b(); // { dg-warning "empty parentheses" }
  unsigned c(); // { dg-warning "empty parentheses" }
  short d(); // { dg-warning "empty parentheses" }
}
