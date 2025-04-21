// PR c++/119863
// { dg-additional-options "-fmodules" }
// { dg-module-cmi A }

export module A;

template<typename>
class T;

template<typename>
class U
{
  template<typename>
  friend class T;
};

template<typename V>
class T
{
  U<V> x = {};
};

export
template<typename V>
T<V> f(V) { return {}; }
