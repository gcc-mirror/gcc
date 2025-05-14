/* { dg-do run } */
/* PR tree-optimization/118922 */

/* Phi-opt would convert:
  <bb 5> [local count: 1014686024]:
  if (h_6 != 0)
    goto <bb 7>; [94.50%]
  else
    goto <bb 6>; [5.50%]

  <bb 6> [local count: 114863530]:
  # h_6 = PHI <0(4), 1(5)>

  <bb 7> [local count: 1073741824]:
  # f_8 = PHI <0(5), h_6(6)>
  _9 = f_8 ^ 1;
  a.0_10 = a;
  _11 = _9 + a.0_10;
  if (_11 != -117)
    goto <bb 5>; [94.50%]
  else
    goto <bb 8>; [5.50%]

into:

  <bb 4> [local count: 59055799]:
  c = d_3;

  <bb 5> [local count: 1073741824]:
  # f_8 = PHI <0(5), 0(4)>
  _9 = f_8 ^ 1;
  a.0_10 = a;
  _11 = _9 + a.0_10;
  if (_11 != -117)
    goto <bb 5>; [94.50%]
  else
    goto <bb 6>; [5.50%]

as it thought the middle bb was empty as there was only a phi node there. */


int a = -117, b, c, e;
void g(int h) {
  int f = 0;
  while (!f + a - -117) {
    f = h == 0;
    if (h == 0)
      h = 1;
  }
}
int main() {
  int d = 8;
  for (; e;)
    d = 0;
  c = d;
  g(0);
}
