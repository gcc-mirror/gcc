// { dg-do compile { target c++11 } }
int
foo ()
{
  int i [[and::bitor, bar::xor_eq, compl::baz(1), bitand::xor_eq(2, 3)]]; // { dg-warning "ignored" }
  int j [[using, using::baz, bar::using, using::using (2)]];		  // { dg-warning "ignored" }
  i = 0;
  j = 0;
  return i + j;
}
