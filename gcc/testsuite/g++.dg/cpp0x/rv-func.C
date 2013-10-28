// PR c++/48457, Core 1238
// { dg-options -std=c++11 }

template<class T>
T&& create();

template<class T, class Arg>
void test() {
  T t(create<Arg>());
  (void) t;
}

void f (void (&)());
void f (void (&&)());

int main() {
  test<void(&)(), void()>();
  test<void(&&)(), void()>();
  // This call should choose the lvalue reference overload.
  // { dg-final { scan-assembler-not "_Z1fOFvvE" } }
  f(create<void()>());
}
