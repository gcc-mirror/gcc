// { dg-do run  }

template <class T>
class Foo
{
public:
    Foo(const T&);
    Foo(const T&, const T&);
};

template <class T>
Foo<T>::Foo(const T& t0)
{
}

template <class T>
Foo<T>::Foo(const T& t0, const T& t1)
{
}

template Foo<int>::Foo(const int& t0);


int main (void) {
  return 0;
}












