// { dg-do run  }
// Check that object call works when there are multiple conversion ops
// returning the same type.

typedef int (*pfn)();

int zero () { return 0; }
int one  () { return 1; }
int two  () { return 2; }

struct A {
  A() { }
  operator pfn () { return one; }
  operator pfn () const { return zero; }
  operator pfn () volatile { return two; }
};

int
main ()
{
  const A a;
  return a();
}
