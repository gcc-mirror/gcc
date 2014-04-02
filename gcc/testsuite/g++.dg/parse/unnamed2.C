// PR c++/30301

template<int T> struct A
{
  union { static int i; };	// { dg-error "static data.*unnamed" }
};
