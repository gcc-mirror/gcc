// { dg-options "" }

template <typename T>
void f() {
 l:
  void *p[] = { &&l };

  goto *p;
}

template void f<int>();
