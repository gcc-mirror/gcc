// PR c++/102547
// { dg-do compile { target c++11 } }
// A version of variadic-partial2.C where partial ordering is performed
// on function templates instead of class templates.

template<int... Vs>
struct vals { };

template<class V, class T>
void f(V, T) { };

template<int V0, int V1, class T>
void f(vals<V0, V1>, T) { };

template<int V0, int V1>
void f(vals<V0, V1>, char) { };

template void f(vals<1, 2>, char); //- "sorry, unimplemented..., ICE"

int main() {
  f(vals<1, 3>{}, 'a'); //- "sorry, unimplemented..., ICE"
}
