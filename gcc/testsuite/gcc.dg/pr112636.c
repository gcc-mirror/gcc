/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize" } */

int a[1], b;
unsigned c;
int main() {
  while (b) {
    if (a[c])
      break;
    c--;
  }
  return 0;
}
