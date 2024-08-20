/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3" } */

extern short a[];
short b;
int main() {
  for (char c = 0; c < 18; c += 1)
    a[c + 0] = b;
}
