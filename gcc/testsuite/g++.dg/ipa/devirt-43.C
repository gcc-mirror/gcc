/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-inline-details -fno-ipa-cp -fno-early-inlining" } */
struct A {virtual int foo () {return 1;}};
struct B {int i; struct A a;};
struct C:A {virtual int foo () {return 2;}};

void dostuff(struct A *);

static void
test (struct A *a)
{
  dostuff (a);
  if (a->foo ()!= 2)
    __builtin_abort ();
}

void
t(struct B *b)
{
  test(&b->a);
}
/* Here b comes externally, but we take speculative hint from type of the pointer that it is
   of type B.  This makes A fully specified and we know C::foo is unlikely.
   FIXME: We could most probably can devirtualize unconditonally because dereference of b in
   &b->a makes the type known.  GIMPLE does not represent this.  */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a speculative target" 1 "inline" { xfail *-*-* } } } */
