template <bool B> struct A { };
template <class T> void f()
{
  A<T::I < T::J>();
}
