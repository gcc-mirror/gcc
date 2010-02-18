// { dg-options "" }

template <typename T>
void f() {
 l:
  void *p[] = { &&l };

  goto *p[0];
}

template void f<int>();
