// PR c++/69392
// { dg-do compile { target c++14 } }

template <typename T>
class Foo {
  public:
    void foo(void) {}
    auto getCallableFoo(void) {
      return
        [ptr = this]() { ptr->foo(); };
    }
};

int main()
{
  Foo<int> f;
  auto callable = f.getCallableFoo();
  callable();
}
