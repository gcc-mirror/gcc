// PR c++/17976
// { dg-do run }

extern "C" void abort();
struct A
{
  static int i;
  A(){}
  ~A(){i++;if(i>1)abort();}
};

int A::i = 0;

A a;
extern A a;

int main()
{
  return 0;
}

