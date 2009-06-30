struct A
{
  void* q;
  short i;
};

union U
{
  char* p;
  struct A a;
};

struct A foo(union U u)
{
  struct A a = { 0, 0 };
  a = u.a;
  return a;
}
