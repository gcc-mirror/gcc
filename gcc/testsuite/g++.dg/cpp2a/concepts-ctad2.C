// PR c++/98611
// { dg-do compile { target c++20 } }

template <class>
struct S {
  template <class T> struct Tmpl { Tmpl(T); };

  template <class T>
    requires requires (T object) { Tmpl{object}; }
  static int f(T);
};

int a = S<int>::f(0);
