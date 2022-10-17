// PR c++/101247
// { dg-do compile { target concepts } }
// A variant of concepts-memtmpl3.C where f is defined outside A's definition.

template <typename> struct A {
  template <typename c> static constexpr bool d = true;
  struct B;
  template <typename> struct C;
};

template <typename a>
struct A<a>::B {
  template <typename c> static void f(c) requires d<c>;
};

template <typename a>
template <typename b>
struct A<a>::C {
  template <typename c> static void f(c) requires d<c>;
  static void g() requires d<b>;
};

int main()
{
  A<void>::B::f(0);
  A<void>::C<int>::f(0);
  A<void>::C<int>::g();
}
