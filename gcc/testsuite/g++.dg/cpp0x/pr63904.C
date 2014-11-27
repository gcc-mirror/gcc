// { dg-do compile { target c++11 } }

template<int N>
struct foo {
    constexpr foo() : a() {}
    int a[N];
};

int main() {
  foo< (foo<1>{}).a[0] > f;
  return 0;
}

