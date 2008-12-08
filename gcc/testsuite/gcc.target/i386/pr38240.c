/* { dg-do compile } */

typedef float V
  __attribute__ ((__vector_size__ (16), __may_alias__));

V __attribute__((target("sse"))) f(const V *ptr) { return *ptr; }

V g(const V *ptr) { return *ptr; }
