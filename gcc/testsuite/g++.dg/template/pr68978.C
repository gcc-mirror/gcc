// PR c++/68978

typedef int int32_t __attribute__((mode (__SI__)));

int32_t i = 0, c = 0, radix = 10, max = 0x7fffffff;

template <typename T> int32_t toi_1() {
  if (max < ((i *= radix) += c))
    return 0;
  return i;
}

template <typename T> int32_t toi_2() {
  if (max < ((i = radix) = c))
    return 0;
  return i;
}

template <typename T> int32_t toi_3() {
  if (max < ((i = radix) += c))
    return 0;
  return i;
}

template <typename T> int32_t toi_4() {
  if (max < ((i += radix) = c))
    return 0;
  return i;
}

template <typename T> int32_t toi_5() {
  if (max < (((i = radix) += (c += 5)) *= 30))
    return 0;
  return i;
}

int32_t x = toi_1<int32_t> ();
int32_t y = toi_2<int32_t> ();
int32_t z = toi_3<int32_t> ();
int32_t w = toi_4<int32_t> ();
int32_t r = toi_5<int32_t> ();
