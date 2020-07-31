// PR c++/96197
// { dg-do compile { target c++14 } }

struct S {
  S* p = this;
};

constexpr S ary[5000] = {};

constexpr int foo() {
  int count = 0;
  for (int i = 0; i < 5000; i++)
    if (ary[i].p != nullptr)
      count++;
  return count;
}

constexpr int bar = foo();
