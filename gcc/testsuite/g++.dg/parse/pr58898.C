// PR c++/58898

template <typename = int>
struct Foo
{
  Foo()
  {
    int t(int()); // Error
  }
};

int main()
{
  int t(int()); // OK
  Foo<> a; // Error
}
