// { dg-additional-options -fmodules }
// { dg-module-cmi M }
// { dg-module-do link }

export module M;

export template <class T> struct A
{
  template <class U> friend void f (U);
};

template <class U>
void f(U u) { }
