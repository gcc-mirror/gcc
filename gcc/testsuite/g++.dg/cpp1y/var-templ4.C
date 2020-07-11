// { dg-do compile { target c++14 } }

typedef int int32_t __attribute__((mode (__SI__)));

template<typename T>
  constexpr int32_t var = sizeof (T);

template<>
  constexpr int32_t var<int32_t> = 100000;

int main ()
{
  static_assert(var<int32_t> == 100000 && var<char> == sizeof(char), "");
}
