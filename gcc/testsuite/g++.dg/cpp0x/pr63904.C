// { dg-do compile { target c++11 } }
// { dg-options "-Wno-pedantic" }

template<int N>
struct foo {
    constexpr foo() : a() {}
    int a[N];
};

int main() {
  foo< (foo<1>{}).a[0] > f;
  return 0;
}

