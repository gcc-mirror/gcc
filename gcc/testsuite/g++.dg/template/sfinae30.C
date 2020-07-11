// PR c++/79706
// { dg-do compile { target c++11 } }

struct A {
  void operator delete(void*) = delete;
private:
  void operator delete[](void*);
};

extern A *p;

template<typename T>
auto foo(T *t) -> decltype(delete t); // { dg-error "use of deleted function" }

template<typename T>
auto bar(T *t) -> decltype(delete[] t); // { dg-error "private within this context" }

void baz() {
  foo<A>(p); // { dg-error "no match" }
  bar<A>(p); // { dg-error "no match" }
}
