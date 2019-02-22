// { dg-module-do run }
// { dg-additional-options "-fmodule-header" }
// { dg-module-bmi {"leg-merge-7_a.H"} }

template<int I> int foo (int i) 
{
  return I == i;
}
