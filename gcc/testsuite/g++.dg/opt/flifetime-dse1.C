// { dg-options "-O3 -fno-lifetime-dse" }
// { dg-do run }

typedef __SIZE_TYPE__ size_t;
inline void * operator new (size_t, void *p) { return p; }

struct A
{
  int i;
  A() {}
  ~A() {}
};

int main()
{
  int ar[1];

  A* ap = new(ar) A;
  ap->i = 42;
  ap->~A();

  if (ar[0] != 42) __builtin_abort();
}
