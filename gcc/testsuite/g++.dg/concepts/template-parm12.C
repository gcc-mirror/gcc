// Conceptized version of template/ttp23.C
// { dg-options "-std=c++17 -fconcepts" }

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
