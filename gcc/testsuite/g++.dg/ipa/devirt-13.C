/* { dg-do run } */
/* Call to foo should be devirtualized because there are no derived types of A.  */
/* { dg-options "-O2 -fdump-ipa-cgraph -fdump-tree-ssa"  } */
namespace {
class A {
public:
  virtual int foo(void)
{
  return 0;
}
};
}
class A a, *b=&a;
main()
{
  return b->foo();
}

/* { dg-final { scan-ipa-dump "Devirtualizing call"  "cgraph"  } } */
/* { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "ssa"} } */
/* { dg-final { cleanup-ipa-dump "cgraph" } } */
/* { dg-final { cleanup-tree-dump "ssa" } } */
