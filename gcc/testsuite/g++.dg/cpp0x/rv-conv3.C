// PR c++/90546
// { dg-do link { target c++11 } }

struct Foo { };
void test(const Foo&) {}
Foo f;
struct Bar {
  template <class T> operator T&&();
};
template<> Bar::operator const Foo&&() {
    return static_cast<Foo&&>(f);
}
int main() {
  test(Bar{});
}
