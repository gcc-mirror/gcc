// PR c++/80150
// { dg-do compile { target c++11 } }

template <typename R, typename... Args>
bool compare_functions(R(*funcA)(Args...), R(*funcB)(Args...), Args... args) {
  return false;
}

int foo(int x) {
  return x;
}

float foo(float x) {
 return x;
}

int main() {
  int a = 10;
  compare_functions<int>(foo, foo, a);
}
