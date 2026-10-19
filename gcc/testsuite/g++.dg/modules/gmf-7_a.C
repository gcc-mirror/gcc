// PR c++/122774
// { dg-additional-options "-fmodules" }

export module M;
export extern "C" int scanf(const char *, ...);
