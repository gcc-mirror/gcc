template<class T>
struct A {
  static T arr[5];
};

template <class T>
T A<T>::arr[5] = { 0, 1, 2, 3, 4 };

main ()
{
  return A<int>::arr[0];
}
