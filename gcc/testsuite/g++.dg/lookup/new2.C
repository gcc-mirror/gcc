// { dg-do compile }
// Reject [de-]allocation functions declared in a namespace, or
//   declared as static.

namespace A {
  void* operator new(__SIZE_TYPE__ s, int* p); // { dg-error "namespace" }
  void  operator delete(void*);                // { dg-error "9:.void A::operator delete\\(void\\*\\). may not be declared within a namespace" }
}

static void* operator new(__SIZE_TYPE__ s, int* p); // { dg-error "static" }
static void  operator delete(void*);                // { dg-error "14:.void operator delete\\(void\\*\\). may not be declared as static" }
