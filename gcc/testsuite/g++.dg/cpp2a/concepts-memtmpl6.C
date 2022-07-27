// PR c++/105842
// { dg-do compile { target c++20 } }

template<class T>
struct S {
  static void func1() requires __is_same(T, int);
  static void func1() requires (!__is_same(T, int));

  static void func2() requires false && false;
  static void func2() requires false;

  template<class...> static void tmpl1() requires __is_same(T, int);
  template<class...> static void tmpl1() requires (!__is_same(T, int));

  template<class... Us> static void tmpl2() requires (sizeof...(Us) == 1);
  template<class... Us> static void tmpl2() requires (sizeof...(Us) == 2);

  static void foo() {
    // Both calls resolve to the first overload at instantiation time.
    func1();
    tmpl1();
  }

  static void bar() {
    // We can check and reject both calls ahead of time since the functions'
    // constraints don't depend on outer template parameters.
    func2(); // { dg-error "no match" }
    tmpl2(); // { dg-error "no match" }
  }
};

int main() {
  S<int>::foo();
}
