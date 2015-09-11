// PR c++/56100
// { dg-options "-Wshadow" }

int foo;  // { dg-message "shadowed declaration" }

struct bar
{
  template <typename T>
  void baz () { int foo; }  // { dg-warning "shadows a global" }
};

int main ()
{
  bar ().baz <int> ();
}
