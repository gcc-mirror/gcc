// PR c++/58649

template<typename> void foo()
{
  E();  // { dg-error "declaration|declared" }    
  enum E {};
}

template void foo<int>();
