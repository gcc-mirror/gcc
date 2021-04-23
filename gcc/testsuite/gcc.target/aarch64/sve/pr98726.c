/* { dg-options "-O3" } */

int a, c;
char b;
int d() {
  a = 0;
  for (; a <= 21; a = (short)a + 1)
    b &= c;
}
