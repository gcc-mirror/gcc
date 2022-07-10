/* { dg-do compile } */
/* { dg-options "-O3" } */

int printf(const char *, ...);
long a;
int b;
volatile int c;
int main() {
  long e = a;
  int f = a;
 L:
  if (b > 0) {
    printf("0");
    goto L;
  }
  if (f) {
    printf("%ld", (long)b);
    goto L;
  }
  e >= b && c;
  return 0;
}
