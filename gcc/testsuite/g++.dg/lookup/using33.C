// { dg-do run }

template <class T>
struct Foo
{
  int k (float) {return 0;}
};

template <class T>
struct Baz
{
  int k (int) {return 1;}
};

template <class T>
struct Bar : Foo<T> , Baz<T>
{
  using Foo<T>::k;
  using Baz<T>::k;
};

int main()
{
  Bar<int> bar;
  return bar.k( 1.0f );
}
