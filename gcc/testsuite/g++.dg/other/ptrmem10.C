// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/37093

template <class C, void (C::*M) ()>
static
void foo(void *obj)
{
  C *p = static_cast<C*>(obj);
  (p->*M)();
}

template <class C>
static void
bar(C *c, void (C::*m) ())
{
  foo<C,m>((void *)c);// { dg-error "(not a valid template arg|pointer-to-member|no matching fun)" }
}

struct S
{
  void baz () {}
};

int
main ()
{
  S a;
  bar(&a, &S::baz);
}
