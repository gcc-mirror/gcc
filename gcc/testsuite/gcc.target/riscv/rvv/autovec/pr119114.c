/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-require-effective-target rv64 } */
/* { dg-add-options riscv_v } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -std=gnu99 -fwhole-program -mrvv-vector-bits=zvl" } */

_Bool a;
short b[18];
long long al;
_Bool e;
char f = 010;
short t[18];
unsigned short w[8][18][18][18];

void
c (_Bool e, char f, short t[], unsigned short w[][18][18][18])
{
  for (int ae = 1; ae < f + 5; ae += 2)
    {
      a -= (_Bool) (t[ae - 1] & t[ae + 3]);
      for (short af = 0; af < 18; af += 2)
	for (_Bool ah = 0; ah < (w[e][1][af][0] > 0); ah = 5)
	  b[af] |= 9;
    }
}

int
main ()
{
  for (int ad = 0; ad < 18; ad++)
    t[ad] = 3;

  c (e, f, t, w);
  al = a;
  if (al != 0)
    __builtin_abort ();
}
