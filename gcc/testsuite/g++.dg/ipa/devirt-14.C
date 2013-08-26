/* No devirtualization happens here, but A::foo should not end up as reachable
   because the constructor of A is unreachable and therefore the virtual
   method table referring to A::foo is optimized out.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-ssa"  } */
class B {
public:
  virtual int foo(void)
{
  return 0;
}
};
namespace {
class A : public B {
public:
  virtual int foo(void)
{
  return 1;
}
};
}
class B a, *b=&a;
main()
{
  if (0)
    {
    class A a;
    a.foo();
    }
  return b->foo();
}

/* { dg-final { scan-tree-dump-not "A::foo" "ssa"} } */
/* { dg-final { cleanup-tree-dump "ssa" } } */
