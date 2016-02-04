/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b;
char c;
void fn1() {
  b = 30;
  for (; b <= 32; b++) {
    c = -17;
    for (; c <= 56; c++)
      a -= 0 == (c || b);
  }
}
