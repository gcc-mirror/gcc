/* { dg-do compile } */
/* { dg-options "-O -fcaller-saves -fschedule-insns2 -fselective-scheduling2 -mtune=core2" } */

union double_union
{
  double d;
  int i[2];
};

void bar (int, ...);

void
foo (double d)
{
  union double_union du = { d };
  while (1)
    {
      du.i[1] -= 0x100000L;
      bar (0, du.d);
      du.d += d;
    }
}
