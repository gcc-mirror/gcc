// { dg-do run { target c++11 } }

int x;
int main() {
  constexpr int&& r = static_cast<int&&>(x);
  r = 1;

  auto a = [] { r = 2; };  // Not an ODR-use of 'r' so no capture needed.
  auto b = [&] { r = 3; };

  a();
  if (r != 2)
    __builtin_abort();

  b();
  if (r != 3)
    __builtin_abort();
}
