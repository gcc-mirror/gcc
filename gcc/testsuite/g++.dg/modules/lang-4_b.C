// PR c++/122019
// { dg-additional-options "-fmodules -Wno-global-module" }
// Language linkage for types, variables, and lazy-loading in extern contexts.

module;

extern "C" enum E { c };
extern "C++" typedef int T;

extern "C++" int foo;  // { dg-message "existing" }
extern "C" int bar;  // { dg-message "existing" }

module M;

extern "C" ns::pthread_once_t x;

E e;
T t;

extern "C" { int use1 = foo; }  // { dg-message "during load" }
extern "C" { int use2 = bar; }  // { dg-message "during load" }
// { dg-error "conflicting language linkage for imported declaration 'int foo'" "" { target *-*-* } 0 }
// { dg-error "conflicting language linkage for imported declaration 'int bar'" "" { target *-*-* } 0 }

extern "C++" int qux;  // { dg-error "conflicting declaration" }
extern "C" int zap;  // { dg-error "conflicting declaration" }
