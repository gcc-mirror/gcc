/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv" } */

int a, b;

void
c(void) {
  if (b)
    b = a / b;
}
