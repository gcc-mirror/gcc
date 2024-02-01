/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

extern int wsize;

typedef unsigned short Posf;
#define NIL 0

void foo (Posf *p)
{
  register unsigned n, m;
  do {
      m = *--p;
      *p = (Posf)(m >= wsize ? m-wsize : NIL);
  } while (--n);
}

/* { dg-final { scan-assembler-times {vid\.v\s+v[0-9]+\s+addi\s+\s*[a-x0-9]+,\s*[a-x0-9]+,\s*-1\s+vrsub\.vx\s+} 1 } } */
