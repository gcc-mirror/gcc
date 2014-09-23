// { dg-do compile { target c++14 } }

template<typename T>
  constexpr int var = sizeof (T);

template<>
  constexpr int var<int> = 100000;

int main ()
{
  static_assert(var<int> == 100000 && var<char> == sizeof(char), "");
}
