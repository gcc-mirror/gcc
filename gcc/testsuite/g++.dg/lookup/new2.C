// { dg-do compile }
// Reject [de-]allocation functions declared in a namespace, or
//   declared as static.

namespace A {
  void* operator new(__SIZE_TYPE__ s, int* p); // { dg-error "namespace" }
  void  operator delete(void*);                // { dg-error "namespace" }
}

static void* operator new(__SIZE_TYPE__ s, int* p); // { dg-error "static" }
static void  operator delete(void*);                // { dg-error "static" }
