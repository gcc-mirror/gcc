/* We shall devirtualize to unreachable.  No anonymous type method should surivve
   reachability.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ssa"  } */
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
  if (0)
    {
    class A a;
    a.foo();
    class B b;
    b.foo();
    }
  return b->foo();
}

/* { dg-final { scan-tree-dump-not "A::foo" "ssa"} } */
/* { dg-final { scan-tree-dump-not "B::foo" "ssa"} } */
/* { dg-final { scan-tree-dump "builtin_unreachable" "ssa"} } */
