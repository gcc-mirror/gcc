// PR c++/105452
// { dg-do compile { target c++11 } }

template <typename T>
struct C {
  int i = 42;
  union {
    T v = i;
    static_assert(sizeof(v) == sizeof(char), "");
  };
};

int main() {
  C<char> x;
  return x.v;
}
