/* { dg-do run } */
/* { dg-options "-O0 -mno-accumulate-outgoing-args" } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-additional-options "-mx32 -maddress-mode=short" { target x32 } } */

void
__attribute__ ((noinline))
test (float x1, float x2, float x3, float x4, float x5, float x6,
      float x7, float x8, float x9, float x10, float x11, float x12,
      float x13, float x14, float x15, float x16)
{
  if (x1 != 91
      || x2 != 92
      || x3 != 93
      || x4 != 94
      || x5 != 95
      || x6 != 96
      || x7 != 97
      || x8 != 98
      || x9 != 99
      || x10 != 100
      || x11 != 101
      || x12 != 102
      || x13 != 103
      || x14 != 104
      || x15 != 105
      || x16 != 106)
    __builtin_abort ();
}

float x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13,
      x14, x15, x16;

int
main ()
{
  x1 = 91;
  x2 = 92;
  x3 = 93;
  x4 = 94;
  x5 = 95;
  x6 = 96;
  x7 = 97;
  x8 = 98;
  x9 = 99;
  x10 = 100;
  x11 = 101;
  x12 = 102;
  x13 = 103;
  x14 = 104;
  x15 = 105;
  x16 = 106;
  test (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13,
	x14, x15, x16);
  return 0;
}
