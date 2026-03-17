// PR c++/124459
// { dg-additional-options "-fmodules -Wunused-but-set-variable" }
// Test diagnostic pragmas in macro maps.

export module M;

#define MACRO(t) \
  _Pragma("GCC diagnostic push") \
  _Pragma("GCC diagnostic ignored \"-Wunused-but-set-variable\"") \
  int x = t; \
  _Pragma("GCC diagnostic pop")

export template <typename T> void foo(T t) {
  MACRO(t);
}
