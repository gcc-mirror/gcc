// Origin PR c++/33255
// { dg-options "-Wunused" } <-- should trigger -Wunused-local-typedefs
// { dg-do compile }

void
test_warn()
{
  typedef int foo; // { dg-warning "locally defined but not used" }
}

struct S
{
    typedef int T;
    S() {}
    S(int) {}
};

template<class T>
struct ST
{
    typedef T type;
    ST (int) {}
    ST () {}
};

template<class T>
void
test0_tmpl(void)
{
    typedef struct ST<T> foo;
    foo(2);
}

int
test0(void)
{
    test0_tmpl<int>();
}

void
test1(void)
{
    typedef int foo;
    ST<foo> a;
}


int
test2(void)
{
    typedef S foo;
    foo::T i = 0;
    return i;
}

template<class T>
void
test3_tmpl(void)
{
    typedef struct ST<int> foo;
    ST<int> v;
    const foo __attribute__((unused))&var = v;
}

void
test3(void)
{
    test3_tmpl<int>();
}

void
test4(void)
{
  typedef int foo;
  int __attribute__((unused))vec[1] = {sizeof (foo)};
}

void
test5(void)
{
  typedef int T0;
  typedef char T1;
  typedef int* T2;
  typedef unsigned T3;
  struct C0 { virtual void f(void) {}};
  struct C1 : C0 {};
  typedef C0 T4;

  int v0 = (T0) 2;
  char __attribute__((unused)) v1 = static_cast<T1> (0);
  if (reinterpret_cast<T2> (&v0));
  unsigned* const c = 0;
  unsigned* __attribute__((unused))v2 = const_cast<T3* const> (c);
  C0 *__attribute__((unused))p0 = 0;
  C1 *p1 = 0;
  p0 = dynamic_cast<T4*> (p1);  
}

void
test6(void)
{
  struct C0 {};
  typedef C0 foo;
  C0 *__attribute__((unused))v = new foo;
}

template<class T, class U>
struct S7
{
  void
  f()
  {
    typedef int foo;
    sizeof(foo);
  }
};

template<class T>
void
test7(void)
{
  typedef typename ST<T>::T bar; // { dg-warning "locally defined but not used" }
  typedef typename ST<T>::T foo; // We shouldn't warn for this one, as
				 // it's used below.
  S7<int, foo> v;
}


template<class T, class U>
void
test8(void)
{
  int f(S7<T, U>);
  void g(int);
  typedef T foo;
  g(f(S7<foo, U>()));
}

int
test9(void)
{
  struct s { typedef int foo;}; // { dg-warning "locally defined but not used" }
  struct t { typedef int bar;};
  t::bar b = 0;
  return b;
}
