// PR c++/88969
// { dg-do compile { target c++2a } }

#include <new>

struct B {
  void operator delete(void*, std::destroying_delete_t);  // { dg-error ".operator delete. takes type .B*." }
};
