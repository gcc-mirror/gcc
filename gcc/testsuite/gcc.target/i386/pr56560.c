/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mvzeroupper -dp" } */

extern void abort (void);

typedef double vec_t __attribute__((vector_size(32)));

struct S { int i1; int i2; int i3; };

extern int bar (vec_t, int, int, int, int, int, struct S);

void foo (vec_t v, struct S s)
{
  int i = bar (v, 1, 2, 3, 4, 5, s);
  if (i == 0)
    abort ();
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
