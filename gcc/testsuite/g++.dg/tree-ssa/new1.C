// PR c++/36633

/* { dg-do compile } */
/* { dg-options "-O2 -Wall -fdump-tree-forwprop1" } */
// No particular reason for choosing forwprop1 dump to look at.

struct B { ~B() {} };
struct D : public B {};
//struct D {};

struct my_deleter
{
  void operator()(D * d)
  {
    //    delete [] d;
  }
};

struct smart_ptr
{
  smart_ptr(D * ptr) : p(ptr) { }
  ~smart_ptr() { d(p); }
  D * p;
  my_deleter d;  
};

int
test01()
{
  smart_ptr p(new D[7]);

  return 0;
}

int main()
{
  test01();
  return 0;
}

/* { dg-final { scan-tree-dump-not "= .* \\+ -" "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
