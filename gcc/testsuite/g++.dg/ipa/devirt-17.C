/* We shall devirtualize to B::foo since it is the only live candidate of an
   anonymous type.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-whole-program"  } */
namespace {
class B {
public:
  virtual int foo(void)
{
  return 0;
}
};
class A : public B {
public:
  virtual int foo(void)
{
  return 1;
}
};
}
class B *b;
void get_me_lost (void *);
main()
{
  int c;
  if (c)
    {
    class A a;
    a.foo();
    }
  else
    {
    b = new (class B);
    b->foo();
	get_me_lost ((void *)&b);
    }
  return b->foo();
}

/* { dg-final { scan-ipa-dump "Devirtualizing" "whole-program"} } */
/* { dg-final { scan-ipa-dump-not "builtin_unreachable" "whole-program"} } */
/* { dg-final { scan-ipa-dump "B::foo" "whole-program"} } */
/* { dg-final { scan-ipa-dump-not "A::foo" "whole-program"} } */
/* { dg-final { cleanup-ipa-dump "whole-program" } } */
