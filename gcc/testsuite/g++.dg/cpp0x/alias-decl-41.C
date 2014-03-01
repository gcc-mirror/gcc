// PR c++/60182
// { dg-require-effective-target c++11 }

class B {};
template <typename> using __allocator_base = B;
template <typename> class F : __allocator_base<int> {};
class C {};
template <typename, typename = F<int> > class G : C {};
template <typename> class D;
class A {
  using Container = G<D<char>>;
  A();
  A(D<char> const &);
  Container m_elements;
};
template <template <class, class> class C, class A = F<D<int>>>
void doSomething(C<D<char>, A> &);
A::A(D<char> const &) : A() { doSomething(m_elements); }
