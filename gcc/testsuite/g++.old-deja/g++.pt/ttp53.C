// { dg-do assemble  }

// Submitted by Erez Louidor Lior <s3824888@techst02.technion.ac.il>

template <typename> class H;
template <typename Target, typename Source>
H<Target> foo(const H<Source>&);

template <typename Type>
class H{

#ifdef OK
public:
#endif
  template<template<class, class> class Caster, typename Source>
  static H<Type> cast(const H<Source>& s);

#ifndef OK
  template <typename Target, typename Source>
  friend H<Target> foo(const H<Source>&);
#endif

};

template <class, class> class caster;

template <typename Target, typename Source>
H<Target> foo(const H<Source>& s){
  return H<Target>::template cast<caster, Source>(s);
}

int main(){
  H<int> i;
  foo<const int>(i);
}
