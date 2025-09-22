// PR c++/122019
// { dg-additional-options "-fmodules -Wno-global-module" }

module;

typedef int pthread_once_t;

export module M;

namespace ns {
  using ::pthread_once_t;
}

// note: non-function types don't have language linkage
extern "C++" enum E { c };
extern "C" typedef int T;

extern "C" int foo;
extern "C++" int bar;

extern "C" int qux;
extern "C++" int zap;
