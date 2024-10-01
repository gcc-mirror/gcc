// { dg-do compile }
// { dg-additional-options "-ftree-vectorize" }

template<typename _Tp>
inline const _Tp&
min(const _Tp& __a, const _Tp& __b)
{
  if (__b < __a)
    return __b;
  return __a;
}

unsigned a;
void i(long b, char c[][4], long d[][4]) {
  for (char e; e; e++)
    for (char f = 0; f < 021; f = b)
      for (signed char g; g < 7; g += ~0)
        for (bool h = 0; h < bool(d[f][f]); h = 1)
          a = c[2][1] - min(c[1][f + 1], c[2][f + 2]);
}
