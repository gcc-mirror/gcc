/* { dg-do compile } */
/* { dg-require-effective-target vect_perm } */

int a, c, d;
long b[6];
void fn1() {
  for (; a < 2; a++) {
    c = 0;
    for (; c <= 5; c++)
      d &= b[a * 3];
  }
}
