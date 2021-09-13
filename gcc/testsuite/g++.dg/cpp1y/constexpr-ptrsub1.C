// PR c++/100209
// { dg-do compile { target c++14 } }

struct A {
  int x = 1;
};

struct B : A {
  int y = 2;
  int z = 3;
  int w = 4;
};

constexpr bool f() {
  B b;
  if (&b.w - &b.x != 3)
    /* Effectively disable this test if the layout of B isn't
       what we expect.  */
    return true;
  const int* w = &b.w;
  return *w-- == 4 && *w-- == 3 && *w-- == 2 && *w-- == 1;
}
static_assert(f(), "");
