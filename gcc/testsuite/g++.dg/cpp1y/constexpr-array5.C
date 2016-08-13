// PR c++/71972
// { dg-do compile { target c++14 } }

typedef int size_t;
template <int N> struct S {
  template <size_t M> constexpr S(const char (&)[M]) : data{} {
    data[0] = data[0];
  }
  char data[N];
};
int main() {
  constexpr S<1> s1("");
}
