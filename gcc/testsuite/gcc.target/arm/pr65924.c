/* { dg-do compile } */
/* { dg-options "-O2 -mthumb" } */

int a, b, c;
int fn1() {
  if (b + a < 0)
    c = 0;
}
