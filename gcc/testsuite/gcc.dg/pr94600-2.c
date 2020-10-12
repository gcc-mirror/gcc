/* { dg-do compile } */
/* { dg-require-effective-target size32plus } */
/* { dg-options "-fdump-rtl-final -O2" } */

/* Unrolled version of pr94600-1.c. */

typedef struct {
  unsigned int f0 : 4;
  unsigned int f1 : 11;
  unsigned int f2 : 10;
  unsigned int f3 : 7;
} t0;

void
bar(void)
{
  t0 a00 = { .f0 = 7, .f1 = 99, .f3 = 1, };
  t0 a01 = { .f0 = 7, .f1 = 251, .f3 = 1, };
  t0 a02 = { .f0 = 8, .f1 = 127, .f3 = 5, };
  t0 a03 = { .f0 = 5, .f1 = 1, .f3 = 1, };
  t0 a04 = { .f0 = 5, .f1 = 1, .f3 = 1, };
  t0 a05 = { .f0 = 5, .f1 = 1, .f3 = 1, };
  __SIZE_TYPE__ base = 0x000a0000;

  *(volatile t0 *) ((base) + 44 + (0) * 4) = a00;
  *(volatile t0 *) ((base) + 44 + (1) * 4) = a01;
  *(volatile t0 *) ((base) + 44 + (2) * 4) = a02;
  *(volatile t0 *) ((base) + 44 + (3) * 4) = a03;
  *(volatile t0 *) ((base) + 44 + (4) * 4) = a04;
  *(volatile t0 *) ((base) + 44 + (5) * 4) = a05;
}

/* { dg-final { scan-rtl-dump-times {\(mem/v} 6 "final" } } */
/* { dg-final { scan-rtl-dump-times {\(set \(mem/v} 6 "final" } } */
