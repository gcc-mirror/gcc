// Test that -fno-weak doesn't break explicit instantiation of static data.
// Special g++ Options: -fno-weak

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
