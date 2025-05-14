/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -march=rv64gcv_zvl256b -mabi=lp64d -mtune=generic-ooo -mrvv-vector-bits=zvl" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O2" "-Og" "-Os" "-Oz" } } */

/* A core routine of x264 which should not spill for OoO VLS build.  */

inline int abs(int i)
{
  return (i < 0 ? -i : i);
}

int x264_sad_16x16(unsigned char *p1, int st1, unsigned char *p2, int st2)
{
    int sum = 0;

    for(int y = 0; y < 16; y++)
      {
	for(int x = 0; x < 16; x++)
	    sum += abs (p1[x] - p2[x]);
	p1 += st1; p2 += st2;
      }

      return sum;
}

/* { dg-final { scan-assembler-not {addi\t[a-x0-9]+,sp} } } */
/* { dg-final { scan-assembler-not {addi\tsp,sp} } } */
