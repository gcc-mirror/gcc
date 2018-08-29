/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O2" } */

/* Test for riscv_extend_comparands patch.  */
extern void asdf(int);
void foo(signed char x) {
  switch (x) {
  case 0: asdf(10); break;
  case 1: asdf(11); break;
  case 2: asdf(12); break;
  case 3: asdf(13); break;
  case 4: asdf(14); break;
  }
}
/* { dg-final { scan-assembler-not "andi" } } */
