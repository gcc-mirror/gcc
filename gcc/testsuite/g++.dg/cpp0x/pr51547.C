// PR c++/51547
// { dg-options "-std=c++11" }

template <class T>
struct vector
{
  T*
  begin()
  { return &member; }

  const T*
  begin() const
  { return &member; }

  T member;
};

struct Bar {
  int x;
};

struct Foo {
  const vector<Bar>& bar() const {
    return bar_;
  }

  vector<Bar> bar_;
};

template <class X>
struct Y {
  void foo() {
    Foo a;
    auto b = a.bar().begin();
    auto&& c = b->x;
  }
};

template <class X>
void foo() {
  Foo a;
  auto b = a.bar().begin();
  auto&& c = b->x;
}

int main() {
  Y<int> p;
  p.foo();
  foo<int>();
}
