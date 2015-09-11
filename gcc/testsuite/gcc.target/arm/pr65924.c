/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O2 -mthumb" } */

int a, b, c;
int fn1() {
  if (b + a < 0)
    c = 0;
}
