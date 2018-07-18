// PR c++/85883
// { dg-options -std=c++17 }

template <typename T1, typename T2>
struct Bar
{
  Bar(T1, T2) { }
};

int
main ()
{
  auto x = Bar(1, 2);
  auto y = new Bar(3, 4);
}
