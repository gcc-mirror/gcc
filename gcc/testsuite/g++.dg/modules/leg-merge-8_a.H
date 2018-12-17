// { dg-module-do run }
// { dg-additional-options "-fmodule-legacy" }
// { dg-module-bmi {"leg-merge-8_a.H"} }

template<int I> struct Tpl
{
  int i;
  Tpl () : i (I){}
  operator int () const 
  {
    return i;
  }
};
