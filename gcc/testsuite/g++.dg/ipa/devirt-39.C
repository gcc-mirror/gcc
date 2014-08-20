// PR c++/61214
/* { dg-options "-O2 -fdump-tree-optimized"  } */

struct Base
{
  virtual ~Base();
  virtual Base* clone() {
    return 0;
  }
};

struct Foo : Base
{
  virtual ~Foo();
  virtual Base* clone() {
    return new Foo();
  }
};

int main()
{
  Base* f = new Foo();
  f->clone();
  return 0;
}

/* { dg-final { scan-tree-dump-not "OBJ_TYPE_REF" "optimized"  } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
