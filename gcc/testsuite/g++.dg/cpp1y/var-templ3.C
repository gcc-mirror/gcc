// { dg-do run }
// { dg-options "-std=c++1y" }

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
