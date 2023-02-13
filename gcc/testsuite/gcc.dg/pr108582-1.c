/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-ccp -fno-tree-dce" } */

/*
  PHI-OPT via match_simplify_replacement used to transform:
  if (_25 != 0)
    goto <bb 8>; [25.00%]
  else
    goto <bb 9>; [75.00%]

  <bb 8> [local count: 11649864]:
  # iftmp.5_13 = PHI <2(7)>
  k_22 = k_11 | iftmp.5_13;

  <bb 9> [local count: 105655256]:
  # g_9 = PHI <1(2), 0(8), g_8(7)>
  # k_12 = PHI <k_20(D)(2), k_22(8), k_11(7)>

into:

  _15 = (int) _25;
  _28 = -_15;
  _4 = _13 & _28;
  _6 = _4 | k_11;

  <bb 8> [local count: 105655256]:
  # g_9 = PHI <1(2), g_8(7)>
  # k_12 = PHI <k_20(D)(2), _6(7)>

Removing the phi-node/assignment of _13.

 */

int a, c, d, e, f;
char b;
int main() {
  int g = 1;
  char h[1] = {0};
  while (a) {
    if (f) {
      b = 0;
      if (d)
        continue;
    }
    if (a < 1) {
      g = 0;
      goto L;
    }
  }
  while (c) {
    char *j = h;
    int k;
  L:
    if (e && !g)
      k |= 2 | (*j < 0);
  }
  return 0;
}
