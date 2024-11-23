// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M }

module;

template <class T> struct A {
  void f() const { }
} __attribute__ ((deprecated ("y tho")));

export module M;

export template <class T>
A<T> a;				// { dg-warning "deprecated" }
