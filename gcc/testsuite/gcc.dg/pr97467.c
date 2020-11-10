/* { dg-do compile } */
/* { dg-options "-Os" } */

int a;
long b;
unsigned int c = 1;

int main () {
  int e;
  for (; c <= 0; c++) {
    int f = 0;
    b = e;
    a = f || b << c;
  }
  return 0;
}
