// PR c++/110057
/* { dg-do-compile } */
/* Virtual calls should be devirtualized because we know dynamic type of object in array at compile time */
/* { dg-options "-O3 -fdump-tree-optimized -fno-inline"  } */

class A
{
public:
  virtual ~A()
  {
  }
};

class B : public A
{
public:
  virtual ~B()
  {
  }
};

int main()
{
  B* ptr = new B[10];
  delete[] ptr;
  return 0;
}

/* { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "optimized"} } */
