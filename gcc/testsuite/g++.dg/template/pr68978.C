// PR c++/68978

int i = 0, c = 0, radix = 10, max = 0x7fffffff;

template <typename T> int toi_1() {
  if (max < ((i *= radix) += c))
    return 0;
  return i;
}

template <typename T> int toi_2() {
  if (max < ((i = radix) = c))
    return 0;
  return i;
}

template <typename T> int toi_3() {
  if (max < ((i = radix) += c))
    return 0;
  return i;
}

template <typename T> int toi_4() {
  if (max < ((i += radix) = c))
    return 0;
  return i;
}

template <typename T> int toi_5() {
  if (max < (((i = radix) += (c += 5)) *= 30))
    return 0;
  return i;
}

int x = toi_1<int> ();
int y = toi_2<int> ();
int z = toi_3<int> ();
int w = toi_4<int> ();
int r = toi_5<int> ();
