// DR 1518
// { dg-do compile { target c++11 } }

struct A {
  explicit A() = default;
};

struct B : A {
  explicit B() = default;
};

struct C {
  explicit C();
};

struct D : A {
  C c;
  explicit D() = default;
};

template<typename T> void f() {
  T t = {};			// { dg-error "explicit" }
}
template<typename T> void g() {
  void x(T t);
  x({});			// { dg-error "explicit" }
}

int main()
{
  f<A>();			// { dg-message "required from here" }
  f<B>();			// { dg-message "required from here" }
  f<C>();			// { dg-message "required from here" }
  f<D>();			// { dg-message "required from here" }

  g<A>();			// { dg-message "required from here" }
  g<B>();			// { dg-message "required from here" }
  g<C>();			// { dg-message "required from here" }
  g<D>();			// { dg-message "required from here" }
}
