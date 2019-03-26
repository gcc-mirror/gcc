// { dg-module-do run }
// { dg-additional-options "-fmodule-header" }
// { dg-module-bmi {} }

template<int I> int foo (int i) 
{
  return I == i;
}
