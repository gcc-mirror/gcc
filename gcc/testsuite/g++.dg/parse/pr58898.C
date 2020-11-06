// PR c++/58898

template <typename = int>
struct Foo
{
  Foo()
  {
    int t(int()); // { dg-warning "parentheses were disambiguated" }
  }
};

int main()
{
  int t(int()); // { dg-warning "parentheses were disambiguated" }
  Foo<> a; // Error
}
