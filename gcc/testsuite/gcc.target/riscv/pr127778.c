/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcb_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gcb_zicond -mabi=ilp32d" { target { rv32 } } } */

  void
  store_4000 (int *p, int v)
  {
    p[1000] = v;
  }

/* { dg-final { scan-assembler-not {\mli} } } */
/* { dg-final { scan-assembler-times {\maddi\M} 1 } } */
/* { dg-final { scan-assembler-times {\msw\M} 1 } } */



