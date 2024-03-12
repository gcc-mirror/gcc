/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fdump-ipa-inline-details -fno-early-inlining" } */
struct A {
  virtual int foo () {return 1;}
  void wrapfoo () {foo();}
  A() {wrapfoo();}
};
inline void* operator new(__SIZE_TYPE__ s, void* buf) throw() {
   return buf;
}
struct B:A {virtual int foo () {return 2;}};

void dostuff(struct A *);

static void
test2 (struct A *a)
{
  dostuff (a);
  if (a->foo ()!= 2)
    __builtin_abort ();
}

static void
test (struct A *a)
{
  dostuff (a);
  static_cast<B*>(a)->~B();
  new(a) B();
  test2(a);
}

int main()
{
  struct B a;
  dostuff (&a);
  test (&a);
}

/* One invocation is A::foo () other is B::foo () even though the type is destroyed and rebuilt in test() */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target\[^\\n\]*A::foo" 2 "inline" { target { ! implicit_constexpr } } } }*/
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target\[^\\n\]*B::foo" 1 "inline"  } } */
