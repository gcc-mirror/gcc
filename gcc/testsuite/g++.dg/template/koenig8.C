// PR c++/40740

template<class T>
T addsome(T v) {
  return v+1;
}

int addsome(int v) {
  return v+2;
}

int main() {
  int i = 0;
  if (addsome(i) != 2)
    return 1;
  if (addsome<>(i) != 1)
    return 2;
  return 0;
}

