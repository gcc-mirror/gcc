// { dg-additional-options -fmodule-header }
// PR 98843 ICE due to inconsistent entity_ary order

template<int I> int Fn () 
{
  return I;
}

template<> inline int Fn<1> () 
{
  return 0;
}
