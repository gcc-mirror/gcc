// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template<int I> struct Tpl
{
  int i;
  Tpl () : i (I){}
  operator int () const 
  {
    return i;
  }
};
