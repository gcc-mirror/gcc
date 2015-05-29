/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fdump-ipa-inline-details -fno-early-inlining" } */
struct A {
  virtual int foo () {return 1;}
  int wrapfoo () {foo();}
  A() {wrapfoo();}
};
struct B:A {virtual int foo () {return 2;}};

void dostuff(struct A *);

static void
test (struct A *a)
{
  dostuff (a);
  if (a->foo ()!= 2)
    __builtin_abort ();
}

main()
{
  struct B a;
  dostuff (&a);
  test (&a);
}
/* Here one invocation of foo is while type is in construction, while other is not.
   Check that we handle that.  */

/* { dg-final { scan-ipa-dump "Second type is base of first" "inline"  } } */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target\[^\\n\]*A::foo" 1 "inline"  } } */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target\[^\\n\]*B::foo" 1 "inline"  } } */
