// { dg-options "-O1" }

template <typename T>
int f(T t) {
  switch (t) {
  case 1:
    return 5;
  case 2:
    return 6;
  case 3:
    return -4;
  case 4:
    return 8;
  case 5:
    return 12;
  case 6:
    return 13;
  default:
    return -27;
  }
}

template int f(int);
