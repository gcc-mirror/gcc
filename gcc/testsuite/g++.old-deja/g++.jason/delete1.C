// { dg-do assemble  }
#include <stddef.h>
struct A {
  virtual void operator delete (void *); // { dg-error "" } virtual delete
  virtual void * operator new (size_t);	 // { dg-error "" } virtual new
};
