// { dg-additional-options "-fmodule-header" }
// { dg-module-bmi {} }

template<int I> struct Tpl
{
  int i;
  Tpl () : i (I){}
  operator int () const 
  {
    return i;
  }
};
