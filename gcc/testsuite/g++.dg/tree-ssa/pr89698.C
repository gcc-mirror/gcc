// { dg-do compile }
// { dg-options "-O -fdump-tree-fre1" }

class A {
    virtual void f(){};
public:
    int x;
    A(int in): x(in) {};
};

class B: public A {
public:
    int y;
    B(int in):A(in-1), y(in) {};
};

void bar(void *);
void test()
{
  B b(2);
  A* bp = &b;
  void* vp = dynamic_cast<void*>(bp);
  bar (vp);
}

// We should be able to constant fold from the virtual table
// the offset added to bp for the dynamic cast and forward
// &b to the argument of bar
// { dg-final { scan-tree-dump "bar \\\(&b" "fre1" } }
