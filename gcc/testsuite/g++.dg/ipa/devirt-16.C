/* We shall devirtualize to unreachable.  No anonymous type method should surivve
   reachability.  */
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
main()
{
  int c;
  if (c)
    {
    class A a;
    a.foo();
    class B b;
    b.foo();
    }
  return b->foo();
}

/* { dg-final { scan-ipa-dump "Devirtualizing" "whole-program"} } */
/* { dg-final { scan-ipa-dump "builtin_unreachable" "whole-program"} } */
/* { dg-final { scan-ipa-dump-not "A::foo" "whole-program"} } */
/* { dg-final { scan-ipa-dump-not "A::foo" "whole-program"} } */
/* { dg-final { cleanup-ipa-dump "whole-program" } } */
