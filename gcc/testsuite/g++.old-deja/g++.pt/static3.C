// { dg-do run }
// On targets that don't support weak symbols, we require an explicit
// instantiation of arr.
// { dg-require-weak "" }

template<class T>
struct A {
  static T arr[5];
};

template <class T>
T A<T>::arr[5] = { 0, 1, 2, 3, 4 };

int main ()
{
  return A<int>::arr[0];
}
