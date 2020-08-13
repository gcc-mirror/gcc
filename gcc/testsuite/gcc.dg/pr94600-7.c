/* { dg-do compile } */
/* { dg-require-effective-target size32plus } */
/* { dg-options "-fdump-rtl-final -O2 -fno-unroll-loops" } */

/* Target-as-parameter version of pr94600-3.c. */

typedef struct {
  unsigned int f0 : 4;
  unsigned int f1 : 11;
  unsigned int f2 : 10;
  unsigned int f3 : 7;
} t0 __attribute__((__aligned__(4)));

static t0 a0[] = {
 { .f0 = 7, .f1 = 99, .f3 = 1, },
 { .f0 = 7, .f1 = 251, .f3 = 1, },
 { .f0 = 8, .f1 = 127, .f3 = 5, },
 { .f0 = 5, .f1 = 1, .f3 = 1, },
 { .f0 = 5, .f1 = 1, .f3 = 1, },
 { .f0 = 5, .f1 = 1, .f3 = 1, },
};

void
foo(volatile t0 *b)
{
  __SIZE_TYPE__ i;
  for (i = 0; i < (sizeof (a0) / sizeof ((a0)[0])); i++) {
    b[11] = a0[i];
  }
}

/* { dg-final { scan-rtl-dump-times {\(mem/v} 1 "final" } } */
/* { dg-final { scan-rtl-dump-times {\(set \(mem/v} 1 "final" } } */
