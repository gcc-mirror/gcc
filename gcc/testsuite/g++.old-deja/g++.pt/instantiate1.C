// { dg-do assemble  }

template <class T>
void f(T t) {}

template void f<int>(int);
template void f<>(long);

template <class T>
struct S
{
  void bar(int) {}
  
  template <class U>
  void baz(U u) {}
};


template struct S<char>;
template void S<int>::bar(int);
template void S<double>::baz<short>(short);
template void S<long>::baz<>(char);
