/* PR target/127461 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-Og -msse2 -mtune=pentiumpro" } */

typedef __attribute__((__vector_size__(2))) char U;
typedef __attribute__((__vector_size__(2))) _Float16 V;
typedef __attribute__((__vector_size__(32))) int W;

V a, b;
extern inline __attribute__((__cold__)) void foo(int, int, double, char ) {
  (union {W w;}){};
  U u = (U)b + (U)a;
  ((union {
    U a;
    char b[];
  })u)
      .b;
}
void main() {
  foo((int){}, (int){}, (int){}, 0);
  for (;;)
    ;
}
