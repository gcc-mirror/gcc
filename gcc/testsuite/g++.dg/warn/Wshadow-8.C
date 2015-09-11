// PR c++/56100
// { dg-options "-Wshadow" }

template <typename T>
struct bar
{
  void baz () { int foo; }
};

int foo;

int main ()
{
  bar <int> ().baz ();
}
