template<class T> struct C1
{
  template<class U> struct C2
  { class Type { }; };
};

template<class T, class U>
void foo(typename C1<T>::C2<U>::Type *) { }
