// { dg-do compile { target c++11 } }

using size_t = decltype(sizeof(0));
template <class T> struct A
{
  using size_type = size_t;
};

template <class T>
void f(size_t, T);

template <class T>
void f(typename A<T>::size_type, T);

int main()
{
  f(1,2);			// { dg-error "ambiguous" }
}
