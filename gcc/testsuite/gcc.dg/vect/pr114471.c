/* { dg-do compile } */

float f1, f0, fa[2];
short sa[2];
void quantize(short s0)
{
  _Bool ta[2] = {(fa[0] < 0), (fa[1] < 0)};
  _Bool t = ((s0 > 0) & ta[0]);
  short x1 = s0 + t;
  _Bool t1 = ((x1 > 0) & ta[1]);
  sa[0] = x1;
  sa[1] = s0 + t1;
}
