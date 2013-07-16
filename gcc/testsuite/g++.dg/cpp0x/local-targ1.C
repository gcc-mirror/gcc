// PR c++/51884
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZN1BIZN3fooIivE3barILb1EEEvvE1CEC1ERKS4_" } }

template<typename TT>
  struct test { static const int value = 0; };
template<int I>
  struct enable_if { typedef void type; };

struct A { virtual void f() {} };
template<typename U> struct B : A { B(); B(const B&); };
template<typename U> B<U>::B() { }
template<typename U> B<U>::B(const B&) { }

template<class T> void g(T) { }

template<typename T, typename = void> struct foo;
template<typename T>
struct foo<T,typename enable_if<test<T>::value>::type>
{
  template <bool P> void bar() {
    struct C { } c;
    B<C> b;
    g(b);
  }
};

int main() {
  foo<int> f;
  f.bar<true>();
}
