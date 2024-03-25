/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target riscv_v } */

#include <assert.h>
int (e) (int g, int h) { return h > 0x10 || g > 0xFFFFFFFF >> h ? g : g << h; }
struct i
{
  int j;
  int l : 1;
};
struct m
{
  char k;
  int n;
};
char o;
char p;
short s;
int q;
struct m r;
int v;
int t;
short z;
long ac;
int ad;
int ae;

static void
ai (struct i bf)
{
  for (; v; v++)
    r.k = 0;
  do
    ac ^= bf.j;
  while (bf.j < 0);
  s = 0;
  if (bf.l)
    q |= 0x800;
}

int
main ()
{
  struct i aw = {0xE00, 1};
  o = 4;
  s = p;
  ai (aw);
  t = 1;
  ++p;
  for (; t <= 7; t++)
    {
      ad &= 1;
      (o &= 1 - e (0x40000012, ++ae)) & (z |= 1);
    }
  for (; r.n;)
    ;
  assert (o == 4);
  return 0;
}
