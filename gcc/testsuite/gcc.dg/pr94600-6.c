/* { dg-do compile } */
/* { dg-require-effective-target size32plus } */
/* { dg-options "-fdump-rtl-final -O2" } */

/* Target-as-parameter version of pr94600-2.c. */

typedef struct {
  unsigned int f0 : 4;
  unsigned int f1 : 11;
  unsigned int f2 : 10;
  unsigned int f3 : 7;
} t0 __attribute__((__aligned__(4)));

void
bar(volatile t0 *b)
{
  t0 a00 = { .f0 = 7, .f1 = 99, .f3 = 1, };
  t0 a01 = { .f0 = 7, .f1 = 251, .f3 = 1, };
  t0 a02 = { .f0 = 8, .f1 = 127, .f3 = 5, };
  t0 a03 = { .f0 = 5, .f1 = 1, .f3 = 1, };
  t0 a04 = { .f0 = 5, .f1 = 1, .f3 = 1, };
  t0 a05 = { .f0 = 5, .f1 = 1, .f3 = 1, };

  b[11+0] = a00;
  b[11+1] = a01;
  b[11+2] = a02;
  b[11+3] = a03;
  b[11+4] = a04;
  b[11+5] = a05;
}

/* { dg-final { scan-rtl-dump-times {\(mem/v} 6 "final" } } */
/* { dg-final { scan-rtl-dump-times {\(set \(mem/v} 6 "final" } } */
