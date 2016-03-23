// PR c++/70347
// { dg-do run { target c++14 } }

union A {
  char a;
  long b = -42;
};

struct B {
  union {
    char a = 10;
    long b;
  };
};

A c1{};
A c2{4};
B c3{};
B c4{{9}};

int main() {
  if (c1.b != -42)
    __builtin_abort ();

  if (c2.a != 4)
    __builtin_abort ();

  if (c3.a != 10)
    __builtin_abort ();

  if (c4.a != 9)
    __builtin_abort ();
}
