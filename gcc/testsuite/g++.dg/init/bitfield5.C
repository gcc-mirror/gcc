// PR c++/51219

struct A
{
  int i;
  int : 8;
};

void foo()
{
  A a = { 0 };
}
