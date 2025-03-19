// PR c++/118673
// { dg-do run { target c++11 } }

#include <initializer_list>

struct ArrayRef {
  const int *Data = nullptr;
  ArrayRef(const int &OneElt) : Data(&OneElt) {}
};

struct Vec
{
  ArrayRef Elts[1];
  Vec(std::initializer_list<ArrayRef> IL)
    : Elts{*IL.begin()}
    {  }
};

[[gnu::noinline]] Vec fn() {
  static const auto extension = 42;
  return {extension};
}
int main() {
  auto t = fn();
  if (t.Elts[0].Data[0] != 42) __builtin_abort();
}
