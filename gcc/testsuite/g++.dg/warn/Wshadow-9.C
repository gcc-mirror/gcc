// PR c++/56100
// { dg-options "-Wshadow" }

int foo;  // { dg-message "shadowed declaration" }

template <typename T>
struct bar
{
  void baz () { int foo; }  // { dg-warning "shadows a global" }
};

int main ()
{
  bar <int> ().baz ();
}
