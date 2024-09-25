// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }
// Conceptized version of template/ttp23.C

template <class T> concept Foo = true;

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
  foo (a);  // { dg-error "call of overloaded" }
}
