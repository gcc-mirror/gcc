/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64imv -mabi=lp64d -mrvv-max-lmul=dynamic" } */

short a[5];
int b() {
  int c = 0;
  for (; c <= 4; c++)
    if (a[c])
      break;
  return c;
}
