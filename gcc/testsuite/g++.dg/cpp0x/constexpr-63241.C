// PR c++/63241
// { dg-do compile { target c++11 } }

struct A {
  constexpr A(int){}
};

int main() {
  int i = 1;
  A array[2][2] =
    {{{0}, {i}},
     {{0}, {0}}};
}
