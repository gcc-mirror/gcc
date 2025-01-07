/* Test that we do not use QI vector to initilize the memory if the
 * size of QI vector isn't larger than UNITS_PER_WORD */
/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */

struct s {
  int a;
  int b : 1;
};

void q(struct s*);

void g() {
  struct s r = { 15, 0 };
  q(&r);
}

/* { dg-final { scan-assembler-times {sw\tzero,12\(sp\)} 1 } } */
