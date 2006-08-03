// PR c++/28250
// { dg-do compile }

void foo()
{
  try { throw; }
  catch () {}  // { dg-error "type-specifier" }
}
