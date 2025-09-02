// Test mangling of C++20 class NTTP objects with implicitly zeroed
// non-trailing subojects.
// PR c++/121231
// { dg-do compile { target c++20 } }

struct A {
  int x, y, z;

  static constexpr A make(int x, int y, int z) {
    A a{};
    if (x != 0)
      a.x = x;
    if (y != 0)
      a.y = y;
    if (z != 0)
      a.z = z;
    return a;
  }
};

struct B : A {
  int w;

  static constexpr B make(int x, int y, int z, int w) {
    B b{};
    if (x != 0 || y != 0 || z != 0)
      static_cast<A&>(b) = A::make(x, y, z);
    if (w != 0)
      b.w = w;
    return b;
  }
};

struct C {
  int xyz[3];

  static constexpr C make(int x, int y, int z) {
    C c{};
    if (x != 0)
      c.xyz[0] = x;
    if (y != 0)
      c.xyz[1] = y;
    if (z != 0)
      c.xyz[2] = z;
    return c;
  }
};

template<int N, A a> void f();
template<int N, B b> void g();
template<int N, C c> void h();

int main() {
  f<0, A::make(0, 0, 1)>();    // void f<0, A{0, 0, 1}>()
  f<1, A::make(0, 1, 0)>();    // void f<1, A{0, 1}>()
  f<2, A::make(0, 0, 0)>();    // void f<2, A{}>()
  f<3, A::make(1, 0, 1)>();    // void f<3, A{1, 0, 1}>()

  g<0, B::make(0, 0, 0, 1)>(); // void g<0, B{A{}, 1}>()
  g<1, B::make(0, 0, 1, 0)>(); // void g<1, B{A{0, 0, 1}}>()
  g<2, B::make(0, 1, 0, 0)>(); // void g<2, B{A{0, 1}}>()
  g<3, B::make(0, 0, 0, 0)>(); // void g<3, B{}>()
  g<4, B::make(1, 0, 1, 0)>(); // void g<4, B{A{1, 0, 1}}>()

  h<0, C::make(0, 0, 1)>();    // void h<0, C{int [3]{0, 0, 1}}>()
  h<1, C::make(0, 1, 0)>();    // void h<1, C{int [3]{0, 1}}>()
  h<2, C::make(0, 0, 0)>();    // void h<2, C{}>()
  h<3, C::make(1, 0, 1)>();    // void h<3, C{int [3]{1, 0, 1}}>()
}

// { dg-final { scan-assembler "_Z1fILi0EXtl1ALi0ELi0ELi1EEEEvv" } }
// { dg-final { scan-assembler "_Z1fILi1EXtl1ALi0ELi1EEEEvv" } }
// { dg-final { scan-assembler "_Z1fILi2EXtl1AEEEvv" } }
// { dg-final { scan-assembler "_Z1fILi3EXtl1ALi1ELi0ELi1EEEEvv" } }

// { dg-final { scan-assembler "_Z1gILi0EXtl1Btl1AELi1EEEEvv" } }
// { dg-final { scan-assembler "_Z1gILi1EXtl1Btl1ALi0ELi0ELi1EEEEEvv" } }
// { dg-final { scan-assembler "_Z1gILi2EXtl1Btl1ALi0ELi1EEEEEvv" } }
// { dg-final { scan-assembler "_Z1gILi3EXtl1BEEEvv" } }
// { dg-final { scan-assembler "_Z1gILi4EXtl1Btl1ALi1ELi0ELi1EEEEEvv" } }

// { dg-final { scan-assembler "_Z1hILi0EXtl1CtlA3_iLi0ELi0ELi1EEEEEvv" } }
// { dg-final { scan-assembler "_Z1hILi1EXtl1CtlA3_iLi0ELi1EEEEEvv" } }
// { dg-final { scan-assembler "_Z1hILi2EXtl1CEEEvv" } }
// { dg-final { scan-assembler "_Z1hILi3EXtl1CtlA3_iLi1ELi0ELi1EEEEEvv" } }
