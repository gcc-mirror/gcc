// PR c++/114078
// { dg-do compile { target c++23 } }

using size_t = decltype(sizeof(0));

struct S {
  void* operator new(this size_t);  // { dg-error "explicit object" }
  void* operator new[](this size_t);  // { dg-error "explicit object" }
  void operator delete(this void*);  // { dg-error "explicit object" }
  void operator delete[](this void*);  // { dg-error "explicit object" }
};
