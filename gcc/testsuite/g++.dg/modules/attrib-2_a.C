// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M }

export module M;

export
{
  struct A { int i; };

  __attribute ((access (none, 1)))
  void f(const A&);
}
