// Special g++ Options: -fvtable-thunks

#include <typeinfo>

int state;
int fail;

class A {
public:
  A() {
    if (++state != 1)
      fail = 1;
  }
  virtual int foo() {
    if (++state != 2)
      fail = 1;
  }
  virtual ~A() {
    if (++state != 3)
      fail = 1;
  }
};

A* bar() {
  return new A;
}

main() {
  A *aptr = bar();
  aptr->foo();
  if (dynamic_cast <void*> (aptr) != aptr)
    fail = 1;
  if (typeid (*aptr) != typeid (A))
    fail = 1;
  delete aptr;
  if (++state != 4)
    fail = 1;
  return fail;
}
