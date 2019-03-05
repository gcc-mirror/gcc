// PR c++/85883
// { dg-do compile { target c++17 } }

template <typename T>
struct Bar
{
  Bar(T) { }
};

int
main ()
{
  auto x = Bar(1);
  auto y = new Bar(3);
}
