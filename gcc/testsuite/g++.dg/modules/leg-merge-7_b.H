// { dg-additional-options "-fmodule-legacy" }
// { dg-module-bmi {"leg-merge-7_b.H"} }

template<int I> int foo (int i) 
{
  return I == i;
}
