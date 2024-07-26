// PR c++/116052
// { dg-do compile { target c++11 } }

template <class T> using decay_t = __decay(T);
template <class T> void g(typename decay_t<T>::foo); // { dg-error "built-in" }
template <class T> void f()
{
  g<T>(0);
}

struct A { using foo = int; };

int main()
{
  f<A>();
}
