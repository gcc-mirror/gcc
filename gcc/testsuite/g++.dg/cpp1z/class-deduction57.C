// PR c++/85883
// { dg-options -std=c++17 }

template <typename T1, typename T2, typename T3>
struct Bar
{
  Bar(T1, T2, T3) { }
};

int
main ()
{
  auto x = Bar(1, 2, 3);
  auto y = new Bar(3, 4, 5);
}
