/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned __attribute__((mode(DI))) uint64_t;

struct S0 {
  uint64_t f1;
  uint64_t f2;
  uint64_t f3;
  uint64_t f4;
  uint64_t f5;
} a;
struct S2 {
  uint64_t f0;
  uint64_t f2;
  struct S0 f3;
};

void fn1 (struct S2 b) {
  a = b.f3;
}

/* { dg-final { scan-assembler-times {ldp\s+q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {stp\s+q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-not {ld[1-3]} } } */
