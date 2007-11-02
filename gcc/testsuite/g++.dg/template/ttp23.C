// PR c++/29236

template <typename T> struct A {};

template <template <typename> class P>
struct B {
    template <template <typename> class Q>
    friend bool foo (const B<Q>& a);
};

template <template <typename> class Q>
bool foo (const B<Q>& a);

void bar () {
  B<A> a;
  foo (a);
}
