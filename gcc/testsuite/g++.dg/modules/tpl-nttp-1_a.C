// PR c++/116382
// { dg-additional-options "-fmodules-ts -std=c++20" }
// { dg-module-cmi m:a }

module m:a;
template <typename> struct X {};
template <X<int> nttp> struct index {};
template struct index<{}>;
