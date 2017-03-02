// { dg-options -std=c++1z }

template <class,class> struct same;
template <class T> struct same<T,T> {};

template <class T = void> struct A { };
template <class T> struct B { B(T,T); };

int main()
{
  same<decltype(new A),A<void>*>();
  same<decltype(new B{1,2}),B<int>*>();
}
