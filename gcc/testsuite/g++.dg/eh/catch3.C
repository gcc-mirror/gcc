// PR c++/28250
// { dg-do compile }

template<int> void foo()
{
  try {}
  catch () {}  // { dg-error "type-specifier" }
}

template void foo<0>();
