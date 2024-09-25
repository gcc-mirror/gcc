/* { dg-additional-options "-Wno-analyzer-use-of-uninitialized-value -Wno-psabi" } */
/* { dg-skip-if "incompatible types" { "avr-*-*" } } */

typedef int __attribute__((__vector_size__(4))) T;
typedef unsigned __attribute__((__vector_size__(4))) U;
typedef unsigned __attribute__((__vector_size__(16))) V;
typedef unsigned long __attribute__((__vector_size__(16))) W;

U u;
T t;

void
foo(W w) {
  U u = __builtin_shufflevector((V)w, u, 0);
  t = (T){} + u + u;
  foo((W){});
  for (;;)
    ;
}
