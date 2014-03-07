// Mixed explicit and implicit generic lambda test.
// { dg-do compile { target c++1y } }

int main()
{
  auto f = [] <typename T> (T a, auto b) { return a + b; };
  auto g = [] <typename T> (auto a, T b) { return a + b; };

  return f (1.0, 3) + g (1.0, 3);
}
