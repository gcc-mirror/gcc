// GROUPS passed operator-new
#include <stdio.h>
#include <stdlib.h>
#include <new>

int pass = 0;

void *operator new(size_t sz) throw (std::bad_alloc) {

  void *p;

  pass = 1;
  p = malloc(sz);
  return p;
}

class A {
public:
  A() {}
  ~A() {}

  int a;
  int b;
};


int main()
{
  A *bb = new A[10];
  delete [] bb;

  if (pass)
    printf ("PASS\n");
  else
    printf ("FAIL\n");
}
