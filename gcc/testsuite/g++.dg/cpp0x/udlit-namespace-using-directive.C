// { dg-do compile { target c++11 } }

int operator""_t(long long unsigned) {
    return 0;
}

namespace foo {
  int operator""_t(long long unsigned) {
    return 0;
  }
}

int main() {
  using foo::operator""_t;
  10_t;
}
