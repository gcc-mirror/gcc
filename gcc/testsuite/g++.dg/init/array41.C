// PR c++/70690
// { dg-do run }

struct A {
  A() {}
};

struct APadded : public A {
  char pad[63];
};

int f();
int i = f();
APadded cache[50];
APadded *p = cache;

int f()
{
  cache[0].pad[0] = 42;
  return 1;
}

int main()
{
  if (cache[0].pad[0] != 42)
    __builtin_abort();
}
