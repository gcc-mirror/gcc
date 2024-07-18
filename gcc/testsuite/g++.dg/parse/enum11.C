// PR c++/58980

template<typename> struct A
{ 
  enum A::B::C {};   // { dg-error "" }
};
