// PR c++/51973
// { dg-do compile { target c++11 } }

template <class T>
void f(T t) { }

template <class T> decltype(f<T>(0)) g();
template <class T> decltype(f<T*>(0)) g();

int main()
{
  g<void>();
}
