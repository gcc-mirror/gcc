// { dg-do compile { target c++14 } }

// Template variables and static member variables of template classes are
// often confused.

template<typename T>
  struct S1
  {
    static int n;
    static int arr[];
  };

template<typename T>
  constexpr int var = sizeof (T);

template<typename T>
  int S1<T>::n = sizeof (T);

template<typename T>
  int S1<T>::arr[sizeof (T)];

template<>
  int S1<int>::n = 8;

template<>
  int S1<int>::arr[8];

int main ()
{
  S1<int> v1;
  var<S1<int>>;
  return 0;
}
