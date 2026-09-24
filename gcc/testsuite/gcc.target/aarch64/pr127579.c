/* { dg-do compile } */
/* { dg-additional-options "-O2 -mgeneral-regs-only -march=armv9-a" } */

char a;
int b;
void e() {
  char *c = (char *)&b;
  int d = 0;
  for (; d < b; d++)
    a += c[d];
}
