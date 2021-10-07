// PR c++/102547
// { dg-do compile { target c++11 } }

template<int... Vs>
struct vals { };

template<class V, class T>
struct vals_client { };

template<int V0, int V1, class T>
struct vals_client<vals<V0, V1>, T> { };

template<int V0, int V1>
struct vals_client<vals<V0, V1>, void> { };

template struct vals_client<vals<1, 2>, void>; //- "sorry, unimplemented..., ICE"
