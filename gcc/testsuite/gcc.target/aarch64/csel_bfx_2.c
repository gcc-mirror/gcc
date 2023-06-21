/* { dg-do compile } */
/* { dg-options "-O2" } */
unsigned
f1(int t, int t1)
{
  int tt = 0;
  if(t)
    tt = (t1&0x8)!=0;
  return tt;
}
struct f
{
  unsigned t:3;
  unsigned t1:4;
};
unsigned
f2(int t, struct f y)
{
  int tt = 0;
  if(t)
    tt = y.t1;
  return tt;
}
/* Both f1 and f2 should produce a csel and not a cbz on the argument. */
/*  { dg-final { scan-assembler-times "csel\t" 2 } } */
/*  { dg-final { scan-assembler-times "ubfx\t" 2 } } */
/*  { dg-final { scan-assembler-not "cbz\t" } } */
