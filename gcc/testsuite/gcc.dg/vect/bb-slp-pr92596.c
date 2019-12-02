/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

typedef struct {
  long n[5];
} secp256k1_fe;

secp256k1_fe a;

void fn1(int p1) { a.n[0] = a.n[1] = a.n[2] = p1; }
void fn2() {
  int b;
  fn1(!b);
}
