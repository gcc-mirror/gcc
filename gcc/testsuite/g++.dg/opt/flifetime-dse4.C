// { dg-options "-O3 -flifetime-dse=1" }
// { dg-do run }
// { dg-xfail-run-if "AIX operator new" { powerpc-ibm-aix* } }

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
  int ar[1] = { 42 };
  A* ap = new(ar) A;

  // With -flifetime-dse=1 we retain the old value.
  if (ap->i != 42) __builtin_abort();

  ap->i = 42;
  ap->~A();

  // When the destructor ends the object no longer exists.
  if (ar[0] == 42) __builtin_abort();
}
