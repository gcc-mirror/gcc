/* { dg-do compile } */
/* { dg-options "-O2 -msoft-mul -msoft-div" } */

int calc (int a, int b, int c) {
  return a * b / c;
}

/* { dg-final { scan-assembler-not "l.mul" } } */
/* { dg-final { scan-assembler-not "l.div" } } */
