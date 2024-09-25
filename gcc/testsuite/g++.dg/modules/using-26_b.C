// PR c++/115798
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi xstd }

export module xstd;
import base;

export namespace std {
  using std::int8_t;
}
