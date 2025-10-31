/* { dg-do compile } */

typedef struct {
  int mant;
  int exp;
} SoftFloat;
SoftFloat __trans_tmp_8, main___trans_tmp_5;
static SoftFloat av_normalize_sf(SoftFloat a) {
  while (a.mant + 536870911 < 1073741823) {
    a.mant += a.mant;
    a.exp -= 1;
  }
  return a;
}
void main() {
  main___trans_tmp_5 = av_normalize_sf((SoftFloat){1, 29 + 1});
  SoftFloat sf1 = main___trans_tmp_5;
  for (;;) {
    int t = main___trans_tmp_5.exp - sf1.exp;
    if (t < 2)
      sf1 = __trans_tmp_8;
  }
}
