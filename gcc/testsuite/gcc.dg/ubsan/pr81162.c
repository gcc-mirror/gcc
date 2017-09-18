/* PR tree-optimization/81162 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -O2" } */

short s;
int i1 = 1;
int i2 = 1;
unsigned char uc = 147;

int main() {
  s = (-uc + 2147483647) << 0;
  if (9031239389974324562ULL >= (-((i1 && i2) + uc) ^ -21096) ) {
    return 0;
  } else {
    return -1;
  }
}
