// Origin: PR c++/47398
// { dg-do compile }

template<int>
struct A
{
  typedef int INT;
};

template<int I>
struct transform
{
  static int bar();
};

template<class T, int a, class U, int b>
struct B
{
  typedef typename A<a>::INT TINT;
  void baz();
};

template<class T, int a, class U>
struct B<T, a, U, 1>
{
  typedef typename A<a>::INT TINT;
  void foo();
};

template<class T, int a, class U, int b>
void
B<T, a, U, b>::baz()
{
  int c = transform<sizeof(TINT)>::bar();//#0
}

template<class T, int a, class U>
void
B<T, a, U, 1>::foo()
{
  int c = transform<sizeof(TINT)>::bar();//#1
}

int
main()
{
  B<int, 2, char, 1> i;
  i.foo();
  // While instantiating
  //
  //   template<class T, int a, class U> void B<T, a, U, 1>::foo()
  //
  // lookup_template_class resolves transform<sizeof(TINT)> in #1 to
  // the wrong one; it picks up the one in #0 instead. This is because
  // to compare the two A<a> comp_template_args uses cp_tree_equal
  // that fails to consider the number of siblings of parm 'a'.
return 0;
}
