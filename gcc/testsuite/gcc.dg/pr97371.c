/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

int a, b;
void c() {
  if (b >> 38)
    a = b;
}
