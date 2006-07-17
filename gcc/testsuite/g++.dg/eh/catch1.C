// PR c++/28250
// { dg-do compile }

template<int> void foo()
{
  try {}
  catch (int(0)) {}  // { dg-error "before" }
}
