struct foo {
    typedef int (*fun)(int);

  static int f(int);    // overload between static & non-static
  int f();

  static int g(int);    // non-overloaded static
};

template<foo::fun>
struct f_obj {
  // something ..
};

int foo::f() {
  f_obj<f> f1;
  f_obj<g> f2;

  return 0; 
}
