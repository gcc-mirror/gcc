// { dg-do compile { target c++11 } }
// PR 78488, seg fault with inherited ctor

struct Foo { Foo() {} };

struct Bar : Foo {
  using Foo::Foo;
  Bar(void*);
};

int main() {
 Bar f;
}
