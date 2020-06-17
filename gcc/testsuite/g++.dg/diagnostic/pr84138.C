// PR c++/84138

char
foo()
{
  const int i = 0 = 0; // { dg-error "lvalue required as left operand" }
  return 1 ? 0 : (char)i;
}
