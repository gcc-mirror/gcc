// { dg-do run  }
// { dg-options "-fno-weak" }
// Test that -fno-weak doesn't break explicit instantiation of static data.

template <class T> struct A
{
  static int i;
};

template <class T> int A<T>::i = 42;

template class A<int>;

int main ()
{
  return (A<int>::i != 42);
}
