template <int I> struct B { };

template <class T>
B<T::I> f();

class A
{
  static const int I = 42;
  template <class T> friend B<T::I> f();
};

int main()
{
  f<A>();
}
