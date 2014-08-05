// { dg-do run }
// { dg-options "-std=c++1y" }

template<typename T>
  constexpr int var = sizeof (T);

template<>
  constexpr int var<int> = 100000;

int main ()
{
  return !(
       var<int> == 100000
    && var<char> == sizeof(char)
  );
}
