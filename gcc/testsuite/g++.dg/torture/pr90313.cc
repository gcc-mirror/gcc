// { dg-do run }

#include <stddef.h>

namespace std {
  template<typename T, size_t N> struct array {
    T elems[N];
    const T &operator[](size_t i) const { return elems[i]; }
  };
}

using Coordinates = std::array<double, 3>;

Coordinates map(const Coordinates &c, size_t level)
{
  Coordinates result{ c[1], c[2], c[0] };

  if (level != 0)
    result = map (result, level - 1);

  return result;
}

int main()
{
  Coordinates vecOfCoordinates = { 1.0, 2.0, 3.0 };

  auto result = map(vecOfCoordinates, 1);
  if (result[0] != 3 || result[1] != 1 || result[2] != 2)
    __builtin_abort ();

  return 0;
}
