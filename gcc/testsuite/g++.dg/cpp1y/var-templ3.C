// { dg-do run { target c++14 } }

template<typename T>
 constexpr int var = sizeof (T);

template<typename T>
  struct S1
  {
    template<typename U>
    static constexpr int a = sizeof (U) + sizeof (T);
  };

int main ()
{
  return !(
    var<int> + var<char> == S1<int>::a<char>
  );
}
