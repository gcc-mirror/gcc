/* { dg-do run } */

extern "C" void abort (void);

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

int test(void)
{
  int res;
  B b(2);
  A* bp = &b;
  void* vp = dynamic_cast<void*>(bp);
  if (((A*)vp)->x == 1 && ((B*)vp)->y == 2)
    return 1;
  return 0;
}
int main() { if (test() != 1) abort (); return 0; }
