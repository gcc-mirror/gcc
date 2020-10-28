/* { dg-do compile } */
/* { dg-require-effective-target size32plus } */
/* { dg-options "-fdump-rtl-final -O2" } */
/* { dg-additional-options "-DALIGN_VAR" { target { ! non_strict_align } } } */

/* Assignments to a whole struct of suitable size (32 bytes) must not be
   picked apart into field accesses. */

typedef struct {
  unsigned int f0 : 4;
  unsigned int f1 : 11;
  unsigned int f2 : 10;
  unsigned int f3 : 7;
} t0;

static t0 a0[]
#ifdef ALIGN_VAR
__attribute__((aligned (4)))
#endif
  = {
 { .f0 = 7, .f1 = 99, .f3 = 1, },
 { .f0 = 7, .f1 = 251, .f3 = 1, },
 { .f0 = 8, .f1 = 127, .f3 = 5, },
 { .f0 = 5, .f1 = 1, .f3 = 1, },
 { .f0 = 5, .f1 = 1, .f3 = 1, },
 { .f0 = 5, .f1 = 1, .f3 = 1, },
};

void
foo(void)
{
  __SIZE_TYPE__ i;
  __SIZE_TYPE__ base = 0x000a0000;
  for (i = 0; i < (sizeof (a0) / sizeof ((a0)[0])); i++) {
    *(volatile t0 *) (base + 44 + i * 4) = a0[i];
  }
}

/* The only volatile accesses should be the obvious writes.  */
/* { dg-final { scan-rtl-dump-times {\(mem/v} 6 "final" } } */
/* { dg-final { scan-rtl-dump-times {\(set \(mem/v} 6 "final" } } */
