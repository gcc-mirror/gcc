// PR c++/103198
// { dg-do compile { target c++20 } }

template<class T, class = void>
struct A {
  T val;

  template<class U>
    requires requires { val.x; }
  void f(U);

  static void g(int)
    requires requires { val.x; };

  void h(int)
    requires requires { val.x; };
};

struct B { int x; };
struct C { };

int main() {
  A<B>().f(0);
  A<B>().g(0);
  A<B>().h(0);

  A<C>().f(0); // { dg-error "no match" }
  A<C>().g(0); // { dg-error "no match" }
  A<C>().h(0); // { dg-error "no match" }
}
