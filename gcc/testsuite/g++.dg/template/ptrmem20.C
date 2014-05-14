// PR c++/43079

struct A {};

struct B
{
  void foo() const;
  void foo();
};

template<void (A::*)()> void bar(); // { dg-message "note" }

void baz()
{
  bar<&B::foo>();  // { dg-error "not a valid template argument|no match" }
}
