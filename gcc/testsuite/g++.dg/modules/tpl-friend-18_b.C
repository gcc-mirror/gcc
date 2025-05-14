// PR c++/119863
// { dg-additional-options "-fmodules" }
// { dg-module-cmi B }

export module B;

// this should not be considered conflicting
template <typename>
class T;
