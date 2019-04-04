// PR c++/61327
// { dg-do compile { target c++11 } }

template<typename... T>
struct A;

template<typename T>
struct A<T>
{
  template<typename U>
  void f(U* u) {
    u->T::g();
  }
};

struct B {
protected:
  void g() { }
};

struct C : B {
  template<typename...> friend struct A;
};

int main()
{
  C c;
  A<B> a;
  a.f(&c);
}
