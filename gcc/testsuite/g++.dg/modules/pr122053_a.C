// PR c++/122053
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M }

module;
struct mytime {
  long a, b;
};
export module M;
export mytime foo();
