#include <stddef.h>
struct A {
  virtual void operator delete (void *); // ERROR - virtual delete
  virtual void * operator new (size_t);	 // ERROR - virtual new
};
