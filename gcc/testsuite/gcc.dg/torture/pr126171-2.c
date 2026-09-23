/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int ah, o, t, q, aq, as, at;

int __attribute__ ((noipa)) n () {
  return 129105662;
}

int a (int ak, int al[1], int am, int bj) {
  int ay = as = -1782460880;
  at = -28702851;
  o = 0 % al[0] + -1 + bj - 1;
  goto az;
ba:
  al[0] = ak - 1;
  am = -1;
  goto bb;
bc:
  t = -(bj - 2147483646);
  ay = ay - (-1 % bj - 2147483647);
  goto bd;
be:
  aq = -1704571560;
  am = am + bj - 2147483646;
  if (am << (n () - 129105632) == 0)
    goto bc;
  goto bf;
bd:
  ak = (-1 ^ al[0]) - 1 + ak + 2147483647;
  if (ak)
    goto be;
  goto bg;
bf:
  bj = -1 - am - 1 + 2147483647;
  o = -1 - o - 1;
  goto ba;
bg:
  ay = 3;
az:
  goto be;
bb:
  q = (ay >> 31) - -ak - 2147483646;
  if (q != -1) return 1;
  return 0;
}

int main () {
  if (a (-1, (int[]) { -1 }, -1, 2147483647) != 0)
    __builtin_abort ();
  return 0;
}
