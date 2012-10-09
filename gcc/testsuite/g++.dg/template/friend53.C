// PR c++/53403

template <typename T>
class Foo
{
  typedef void type;
  template <typename U> friend void f();
public:
  Foo() {}
};

template class Foo<void>;

template <typename T>
void f()
{
  typedef Foo<void>::type type;
}

int main()
{
  f<void>();
}
