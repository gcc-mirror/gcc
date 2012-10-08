// { dg-do compile { target c++11 } }
int
foo ()
{
  int i [[and, bitor, xor_eq, compl, bitand]]; // { dg-warning "ignored" }
  i = 0;
  return i;
}
