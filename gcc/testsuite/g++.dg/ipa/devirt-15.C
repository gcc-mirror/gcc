/* Check that we speculatively devirutalize call to FOO to B::foo becuase
   A is noreturn.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-devirt-details -fdump-tree-optimized"  } */
/* { dg-add-options bind_pic_locally } */

class A {
public:
  virtual int foo(void)
    {
      throw (1);
      return 0;
    }
};


class B : public A {
public:
  virtual int foo(void);
};

int
B::foo(void)
{
  return 1;
}
class A a, *b=&a;
void
m(void)
{
  b->foo();
}
main()
{
  m();
}

/* { dg-final { scan-ipa-dump "speculatively devirtualizing call" "devirt"} } */
/* Match if (PROF_6 == foo) to verify that the speculation survived.  */
/* { dg-final { scan-tree-dump "== foo" "optimized"} } */
