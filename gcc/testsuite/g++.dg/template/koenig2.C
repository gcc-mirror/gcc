namespace nsp_foo {

  struct A {};

  struct foo {};

}

namespace nsp_bar {

  void foo(nsp_foo::A) {}

  template <class T>
  void bar(T t)
  {
    nsp_bar::foo(t); // line 16
  }

}

int main()
{
  nsp_bar::bar(nsp_foo::A());
}

