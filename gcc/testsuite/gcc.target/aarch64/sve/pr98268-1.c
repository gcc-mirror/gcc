/* { dg-do link } */
/* { dg-options "-flto -O -ftree-vectorize --param=aarch64-autovec-preference=prefer-asimd" } */
/* { dg-additional-sources "pr98268-2.c" } */

short d, e;
void f(char, long*);
int main() {
  long x;
  f(-114, &x);
  return d == e;
}
