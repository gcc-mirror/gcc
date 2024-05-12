// PR c++/114947
// { dg-additional-options "-fmodules-ts -std=c++20" }
// { dg-module-cmi M:part }
module M:part;

template <typename> struct R {};
template <typename T> requires false struct R<T> {};
template <typename T> requires true struct R<T>;
