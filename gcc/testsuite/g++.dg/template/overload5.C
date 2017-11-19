// PR c++/22621

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

f_obj<&foo::f> a;   // OK
f_obj<foo::f>  b;   // OK (note: a and b are of the same type)

int foo::f()
{
  f_obj<&foo::f> a;   // OK
  f_obj<foo::f>  b;   // ERROR: foo::f cannot be a constant expression

  f_obj<&foo::g> c;   // OK
  f_obj<foo::g>  d;   // OK
  return 0;
}

