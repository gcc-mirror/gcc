/* { dg-do run } */
/* { dg-options "-O" } */

struct S0 {
  signed f4;
  signed f9 : 5;
} a[6][5], b = {2}

;
int c, d;
int fn1() {
  struct S0 e[5][6];
  struct S0 f;
  b = f = e[2][5] = a[5][0];
  if (d)
    ;
  else
    return f.f9;
  e[c][45] = a[4][4];
}

int main() {
  fn1();
  if (b.f4 != 0)
    __builtin_abort ();
  return 0;
}
