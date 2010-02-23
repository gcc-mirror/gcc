// PR c++/43143
// { dg-options "-std=c++0x" }

template<typename T>
T&& declval();

template<class T, class... Args>
void test() {
  T t(declval<Args>()...);
}

int main() {
  test<const int>(); // OK
  test<int[23]>(); // Error
}
