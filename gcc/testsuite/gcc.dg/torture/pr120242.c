/* { dg-do run } */
/* { dg-additional-options "-fsigned-char -fno-strict-aliasing -fwrapv" } */

char f1(char a, char b) {
  return b == 0 ? a : b;
}
int f2(int a, int b) {
  return b ? a : 0;
}
struct l {
  unsigned m;
  int n;
};
struct l ae;
char af = -2;
unsigned ah = 4;
int aj = 8;
int *test = &aj;
int main() {
ao:
  if (f2(f1(4, af++), *test) <= 0) {
    for (; ae.n; ae.n++)
      ;
    if (ah)
      goto ao;
  }
  if (af != 1)
    __builtin_abort ();
  __builtin_exit (0);
}
