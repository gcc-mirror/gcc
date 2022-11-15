// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }
// Conceptized version of template/ttp23.C

template <class T> concept bool Foo = true;

template <typename T> struct A {};

template <template <Foo> class P>
struct B {
    template <template <Foo> class Q>
    friend bool foo (const B<Q>& a);
};

template <template <typename> class Q>
bool foo (const B<Q>& a);

void bar () {
  B<A> a;
  foo (a);
}
