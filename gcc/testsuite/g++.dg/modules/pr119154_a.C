// PR c++/119154
// { dg-additional-options "-fmodules" }
// { dg-module-cmi foo }

export module foo;
extern "C++" inline __attribute__((__gnu_inline__)) void bar() {}
