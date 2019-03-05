/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { *-*-* }  { "-mcmov" } { "" } } */

int cond (int a, int b) {
  return a > b;
}

/* { dg-final { scan-assembler-not "l.cmov" } } */
