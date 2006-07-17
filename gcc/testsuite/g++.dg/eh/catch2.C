// PR c++/28250
// { dg-do compile }

void foo()
{
  try {}
  catch () {}  // { dg-error "before" }
  catch (...) {}
}
