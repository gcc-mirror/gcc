/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=scalable -Ofast -fno-vect-cost-model" } */

int a;
void b() {
  long *c = 0;
  int *d;
  for (; a; ++a)
    c[a] = d[-a];
}

/* { dg-final { scan-assembler-times {vrgather} 1 } } */
