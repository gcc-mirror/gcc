// PR c++/119154
// { dg-additional-options "-fmodules" }
// { dg-module-cmi foo }

export module foo;
export extern "C++" inline __attribute__((__gnu_inline__)) void bar() {}
export extern "C++" inline __attribute__((__gnu_inline__)) void decl();
