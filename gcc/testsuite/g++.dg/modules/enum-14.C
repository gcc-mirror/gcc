// PR c++/116929
// { dg-require-effective-target lto }
// { dg-additional-options "-flto -fmodules-ts -Wno-global-module" }

module;
enum { e };
void hexdump() {}
