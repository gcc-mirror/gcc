/* { dg-do compile } */
/* { dg-options "-O1" } */


volatile int a;
void b(int *c) {
  int *d = 0;
  *c = 0;
  *d = 0;
  __builtin_abort();
}
int main() {
  int f;
  if (a)
    b(&f);
  return 0;
}
