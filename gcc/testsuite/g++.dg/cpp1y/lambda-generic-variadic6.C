// PR c++/78816
// { dg-do compile { target c++14 } }

void f(void (*f1)(int)) {
  f1(42);
}

template <typename Lambda>
static auto callback(Lambda &&l)
{
  static auto* p = &l;
  p = &l;
  return [](auto... x){ return (*p)(x...); };
}

int main() {
  int x = 5;
  f(callback([=](int y){}));
}
