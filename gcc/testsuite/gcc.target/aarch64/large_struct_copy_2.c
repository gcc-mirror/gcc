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

void fn1 () {
  struct S2 b = {0, 1, 7, 4073709551611, 4, 8, 7};
  a = b.f3;
}

/* { dg-final { scan-assembler-times {ldp\s+x[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {stp\s+x[0-9]+} 2 } } */
/* { dg-final { scan-assembler-not {ld[1-3]} } } */
