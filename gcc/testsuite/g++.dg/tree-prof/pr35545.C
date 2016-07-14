// devirt.cc
/* { dg-options "-O2 -fdump-ipa-profile_estimate -fdump-tree-optimized" } */

class A {
public:
  virtual int foo() {
     return 1;
  }

int i;
};

class B : public A
{
public:
  virtual int foo() {
     return 2;
  }

 int b;
} ;


int main()
{
 int i;

  A* ap = 0;

  for (i = 0; i < 1000000; i++)
  {

     if (i%7==0)
     {
        ap = new A();
     }
     else
        ap = new B();

    ap->foo();

    delete ap;

  }

  return 0;

}
/* { dg-final-use { scan-ipa-dump "Indirect call -> direct call" "profile_estimate" } } */
/* { dg-final-use { scan-tree-dump-not "OBJ_TYPE_REF" "optimized" } } */
