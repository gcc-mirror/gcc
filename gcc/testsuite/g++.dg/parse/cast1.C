// PR c++/13536
// { dg-require-effective-target ilp32 } */
// { dg-options "-w" }

#include <typeinfo>

void f() {
  (int((char*)0));
  sizeof ((int((char*)0)));
  typeid ((int((char*)0)));
}
