// PR c++/48593

template <typename T> struct foo
{
  T data;
};

template<typename T> struct bar: public foo<T>
{
  void some_func()
  {
    T* ptr = &(foo<T>::data);
  }
};

int main()
{
  bar<int>().some_func();
}

