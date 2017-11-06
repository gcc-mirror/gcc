// PR c++/65579
// { dg-do link { target c++11 } }

template <typename>
struct S {
    int i;
};

struct T {
  static constexpr S<int> s = { 1 };
};

int main()
{
  return T::s.i;
}
