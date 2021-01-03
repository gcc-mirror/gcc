/* { dg-do compile } */
/* { dg-additional-options "-Os -fallow-store-data-races" } */

short a=0;
unsigned long *volatile *volatile *volatile *b;
unsigned long *volatile *volatile *volatile **c[7];
void d() {
  short e=0;
  for (; a;) {
    e = 0;
    for (; e < 7; e++)
      c[e] = &b;
  }
}
int main() { return 0; }
