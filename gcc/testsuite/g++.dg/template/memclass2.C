namespace ns {
  template<typename T>
  struct Foo {
    template<typename U> struct Bar;
  };
  
  template<typename T>
  template<typename U>
  struct Foo<T>::Bar {
    template<typename V> struct Baz;
  };
  
  template<typename T>
  template<typename U>
  template<typename V>
  struct Foo<T>::Bar<U>::Baz {
    Foo<T> chokes;
    ns::Foo<T> works;
  };
}
