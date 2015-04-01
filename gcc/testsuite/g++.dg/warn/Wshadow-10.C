// PR c++/56100
// { dg-options "-Wshadow" }

struct bar
{
  template <typename T>
  void baz () { int foo; }
};

int foo;

int main ()
{
  bar ().baz <int> ();
}
