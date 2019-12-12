/* { dg-do compile } */
/* { dg-options "-O2 -msoft-div" } */

int calc (int a, int b, int c) {
  return a * b / c;
}

/* { dg-final { scan-assembler "l.mul" } } */
/* { dg-final { scan-assembler-not "l.div" } } */
