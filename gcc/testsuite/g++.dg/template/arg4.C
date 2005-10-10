// PR c++/23437

template <void (*p)()> struct S {
  static const int i = 10;
};

void g();

int a[S<g>::i];
